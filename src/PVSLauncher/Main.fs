// Copyright (c) 2021 Masashi Fujita. All rights reserved.

namespace PVSLauncher

open System
open System.IO
open System.Reflection
open System.Text
open Serilog
open Serilog.Core
open Serilog.Events
open Serilog.Sinks.SystemConsole.Themes

module AssemblyInfo =

    let metaDataValue (mda: AssemblyMetadataAttribute) = mda.Value

    let getMetaDataAttribute (assembly: Assembly) key =
        assembly.GetCustomAttributes(typedefof<AssemblyMetadataAttribute>)
        |> Seq.cast<AssemblyMetadataAttribute>
        |> Seq.find (fun x -> x.Key = key)

    let getReleaseDate assembly =
        "ReleaseDate"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getGitHash assembly =
        "GitHash"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getVersion assembly =
        "AssemblyVersion"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let assembly = lazy (Assembly.GetEntryAssembly())

    let printVersion () =
        let version = assembly.Force().GetName().Version
        printfn "%A" version

    let printInfo () =
        let assembly = assembly.Force()
        let name = assembly.GetName()
        let version = assembly.GetName().Version
        let releaseDate = getReleaseDate assembly
        let githash = getGitHash assembly
        printfn "%s - %A - %s - %s" name.Name version releaseDate githash

module CompilationDatabase =
    open FSharp.Json

    type Item =
        { [<JsonField("directory")>]
          Directory: string
          [<JsonField("file")>]
          File: string
          [<JsonField("command")>]
          Command: string option
          [<JsonField("args")>]
          Args: string [] option }

    let readFrom (f: string): Item [] =
        use sr = new StreamReader(f)
        let s = sr.ReadToEnd()
        Json.deserialize<Item []> (s)

    let getFusedCommand (x: Item): string =
        match x with
        | { Command = Some cmd } -> cmd
        | { Args = Some args } -> args |> String.concat " "
        | _ -> failwith "invalid item"

module Main =
    open Argu
    open CommandLine
    open Serilog
    open Serilog.Configuration
    open DotNet.Globbing

    open PVSLanucher.Ninja

    let defaultOutputDirectory = "00.BUILD"
    let defaultLicenseFile = "pvs.lic"

    let private toNativePath (x: string): string =
        if Path.AltDirectorySeparatorChar = Path.DirectorySeparatorChar
        then x
        else x.Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar)

    let rec private buildCPPArguments' (accum: ResizeArray<string>) (args: Span<string>): string [] =
        let hasSeparateOption (x: string) =
            [| "-o"; "-isysroot"; "-isystem" |]
            |> Array.exists (x.StartsWith)

        if args.Length = 0 then
            accum.ToArray()
        else
            let head = args.[0]

            if head.StartsWith("-c")
               || head.StartsWith("-ginlined-scope")
               || head.StartsWith("-fprofile-instr") then
                buildCPPArguments' accum (args.Slice 1)
            elif hasSeparateOption head && 1 < args.Length then
                if head = "-o" then
                    buildCPPArguments' accum (args.Slice 2)
                else
                    accum.Add(head)
                    accum.Add(args.[1])
                    buildCPPArguments' accum (args.Slice 2)
            elif head.StartsWith("-") then
                accum.Add(head)
                buildCPPArguments' accum (args.Slice 1)
            else
                accum.Add(head)
                buildCPPArguments' accum (args.Slice 1)

    let buildCPPArguments (args: string []): string [] =
        let r = ResizeArray<string>(args.Length)
        buildCPPArguments' r (args.AsSpan())

    type EmitContext =
        { OutputDirectory: string
          RootDirectory: string
          LicenseFile: string
          Excludes: Glob [] }

    let emitNinja (ctx: EmitContext) (items: seq<CompilationDatabase.Item>) =
        let getOutputPath (src: string): string =
            src
            // Constructs the relative path from the `rootDir`
            |> fun p -> Path.GetRelativePath(ctx.RootDirectory, p)
            // Combine it to the outDir
            |> fun p -> Path.Join(ctx.OutputDirectory, p)

        let buildRule =
            { Rule.def with
                  Name = "BUILD"
                  Command = [| "$COMMAND" |]
                  DependencyFormat = GCC
                  DependencyFile = "$DEP_FILE" |> Some }

        let analyzeRule =
            { Rule.def with
                  Name = "ANALYZE"
                  Command =
                      [| "$PVS_STUDIO"
                         "--cfg $PVS_CONFIG"
                         "--sourcetree-root $SOURCE_ROOT"
                         "--output-file $out"
                         "--i-file $in"
                         "--source-file $SOURCE_FILE" |] }

        let mergeLogsRule =
            { Rule.def with
                  Name = "MERGE_LOGS"
                  // TODO: Actually, the following command failed to execute
                  //       PlogConverter does not accepts a response file.
                  Command = [| "$PLOG_CONVERTER @${out}.rsp --renderTypes=Plog --outputDir $OUTPUT_DIR" |]
                  ResponseFile = Some("${out}.rsp", "$in_newline") }

        let mutable targets = Set.empty<string>
        let logFiles = ResizeArray<string>()

        let emit (output: TextWriter) (item: CompilationDatabase.Item) =
            let cmd =
                item
                |> CompilationDatabase.getFusedCommand
                |> split
                |> buildCPPArguments

            let isIncluded (x: string): bool =
                if ctx.Excludes
                   |> Seq.exists (fun ex -> ex.IsMatch(x)) then
                    Log.Debug("input {src} is excluded", x)
                    false
                else
                    true

            let emitOne (inputPath: string): unit =
                let outBase = inputPath |> getOutputPath
                let outPath = sprintf "%s.i" outBase

                let hasProcessed (x: string): bool =
                    if targets |> Set.contains x then
                        Log.Debug("output {dst} is already handled", x)
                        true
                    else
                        false

                if (outPath |> hasProcessed |> not) then
                    targets <- Set.add outPath targets

                    let depFile = sprintf "%s.d" outPath
                    let spc = " "

                    let cmdLine =
                        [| $"{cmd |> String.concat spc}"
                           $"-working-directory={item.Directory}"
                           "-MD"
                           $"-MF {depFile}"
                           $"-MT {outPath}"
                           "-E"
                           $"-o {outPath}" |]
                        |> String.concat " "

                    let vars =
                        [ ("DEP_FILE", depFile)
                          ("COMMAND", cmdLine) ]
                        |> Map.ofList

                    let build =
                        { Build.def with
                              Rule = buildRule
                              Inputs = [| inputPath |]
                              Outputs = [| outPath |]
                              Variables = vars }

                    output.WriteLine(build.Render())

                    let analyze =
                        { Build.def with
                              Rule = analyzeRule
                              Inputs = [| outPath |]
                              Outputs = [| $"{outBase}.log" |]
                              Variables = [ ("SOURCE_FILE", inputPath) ] |> Map.ofList }

                    output.WriteLine(analyze.Render())

                    logFiles.Add(analyze.Outputs.[0])

            let isSourceFile (x: string) =
                [| ".c"; ".cpp"; ".cxx"; ".cc" |]
                |> Seq.exists (x.EndsWith)

            match cmd |> Seq.tryFind isSourceFile with
            | Some input when isIncluded input && File.Exists input -> emitOne input
            | _ -> ()

        if ctx.OutputDirectory |> Directory.Exists |> not then
            Directory.CreateDirectory ctx.OutputDirectory
            |> ignore

        let writeNinja (cfgPath: string) (p: string) =
            let tmpNinjaPath =
                Path.Join(ctx.OutputDirectory, Path.GetRandomFileName())

            try
                use tmpWriter = File.CreateText tmpNinjaPath
                tmpWriter.NewLine <- "\n"
                Log.Logger.Information("temporary Ninja file is {temp}", tmpNinjaPath)
                // TODO: Should be configurable.
                tmpWriter.WriteLine("PVS_STUDIO = C:/tools/PVS-Studio/x64/PVS-Studio")
                // TODO: Should be configurable.
                tmpWriter.WriteLine("PLOG_CONVERTER = C:/tools/PVS-Studio/PlogConverter")
                tmpWriter.WriteLine("PVS_CONFIG = {0}", cfgPath)
                tmpWriter.WriteLine("SOURCE_ROOT = {0}", ctx.RootDirectory)
                tmpWriter.WriteLine()
                tmpWriter.WriteLine(buildRule.Render())
                tmpWriter.WriteLine()
                tmpWriter.WriteLine(analyzeRule.Render())
                tmpWriter.WriteLine()
                tmpWriter.WriteLine(mergeLogsRule.Render())
                tmpWriter.WriteLine()
                items |> Seq.iter (emit tmpWriter)
                tmpWriter.WriteLine()
                tmpWriter.Write("build all.plog : MERGE_LOGS ")

                tmpWriter.WriteLine
                    (logFiles
                     |> Seq.map escapeIfNeeded
                     |> String.concat " ")

                tmpWriter.Close()
                Log.Logger.Information("rename {temp} to {output}", tmpNinjaPath, p)
                File.Move(tmpNinjaPath, p, true)
            finally
                if tmpNinjaPath |> File.Exists then File.Delete(tmpNinjaPath)

        let writeConfig (p: string) =
            let tmpCfgPath =
                Path.Join(ctx.OutputDirectory, Path.GetRandomFileName())

            try
                use tmpWriter = File.CreateText tmpCfgPath
                tmpWriter.NewLine <- "\n"
                Log.Logger.Information("temporary config file is {temp}", tmpCfgPath)
                tmpWriter.WriteLine("platform = x64")
                tmpWriter.WriteLine("preprocessor = clang")
                tmpWriter.WriteLine("language = c++")
                tmpWriter.WriteLine("skip-cl-exe = yes")
                tmpWriter.WriteLine("lic-file = {0}", ctx.LicenseFile)
                tmpWriter.Close()
                Log.Logger.Information("rename {temp} to {output}", tmpCfgPath, p)
                File.Move(tmpCfgPath, p, true)
            finally
                if tmpCfgPath |> File.Exists then File.Delete(tmpCfgPath)

        let cfgPath =
            Path.Join(ctx.OutputDirectory, "pvs.config")

        cfgPath |> writeConfig

        Path.Join(ctx.OutputDirectory, "build.ninja")
        |> writeNinja cfgPath

    // printfn "%s\n" (buildRule.Render())
    // items |> Seq.iter emit

    type CLIArguments =
        | Info
        | Version
        | Log_Level of LogEventLevel
        | Output_Directory of path: string
        | License_File of path: string
        | Exclude of pattern: string
        | [<Mandatory; AltCommandLine("-S")>] Source_Directory of path: string
        // fsharplint:disable-next-line UnionCaseNames
        | [<MainCommand; Mandatory>] CompilationDB of file: string
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Info -> "More detailed information"
                | Version -> "Version of application"
                | Log_Level _ -> "Specify the logging level"
                | Output_Directory _ -> $"Use <path> as an output directory (default: {defaultOutputDirectory})"
                | License_File _ -> $"Specify PVS-Studio license file (default: {defaultLicenseFile})"
                | Exclude _ -> "Pattern for excluding files/directories (accepts multiple)"
                | Source_Directory _ -> "Root of the source directory"
                | CompilationDB _ -> "Use <file> as a compilation database"

    [<EntryPoint>]
    let main (argv: string array) =

        let errorHandler =
            ProcessExiter
                (colorizer =
                    function
                    | ErrorCode.HelpText -> None
                    | _ -> Some ConsoleColor.Red)

        let parser =
            ArgumentParser.Create<CLIArguments>(programName = "PVSLauncher", errorHandler = errorHandler)

        let results = parser.Parse(argv)

        if results.IsUsageRequested then
            fprintfn stderr "%s\n" (parser.PrintUsage())
            exit 1

        let levelSw = LoggingLevelSwitch()

        levelSw.MinimumLevel <- results.GetResult(Log_Level, defaultValue = LogEventLevel.Warning)

        Log.Logger <-
            (new LoggerConfiguration())
                .MinimumLevel.ControlledBy(levelSw)
                .WriteTo.Console(theme = AnsiConsoleTheme.Code)
                .CreateLogger()

        if results.Contains Version then
            AssemblyInfo.printVersion ()
            exit 0

        if results.Contains Info then
            AssemblyInfo.printInfo ()
            exit 0

        let excludes = results.GetResults(Exclude)

        let excluder =
            excludes
            |> Seq.map (toNativePath >> Glob.Parse)
            |> Seq.toArray

        let db =
            results.GetResult(CompilationDB, defaultValue = "compile_commands.json")

        let items = CompilationDatabase.readFrom (db)
        Log.Information("total {items} entries found in {db}", items.Length, db)
        let srcDir = results.GetResult(Source_Directory)
        Log.Information("source directory is {srcDir}", srcDir)

        let outDir =
            Path.GetFullPath
                (results.GetResult(Output_Directory, defaultValue = defaultOutputDirectory),
                 Directory.GetCurrentDirectory())

        Log.Information("output directory is {outDir}", outDir)

        let license =
            Path.GetFullPath
                (results.GetResult(License_File, defaultValue = defaultLicenseFile), Directory.GetCurrentDirectory())

        items
        |> emitNinja
            { OutputDirectory = outDir
              RootDirectory = srcDir
              LicenseFile = license
              Excludes = excluder }

        0
