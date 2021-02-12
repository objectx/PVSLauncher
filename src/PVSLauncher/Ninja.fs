module PVSLanucher.Ninja

open System.Text

type DependencyFileFormat =
    | MSVC
    | GCC
    member self.AsString =
        match self with
        | MSVC -> "msvc"
        | GCC -> "gcc"

    override self.ToString() = self.AsString
    static member def = GCC

type Rule =
    { Name: string
      Command: string []
      Description: string option
      DependencyFormat: DependencyFileFormat
      DependencyFile: string option
      ResponseFile: (string * string) option }
    member self.Render(): string =
        let sep = " "

        let statements =
            seq {
                $"rule {self.Name}"

                if self.Description.IsSome
                then $"    description = {self.Description.Value}"

                $"    command = {self.Command |> String.concat sep}"

                if self.DependencyFile.IsSome then
                    $"    deps = {self.DependencyFormat.AsString}"
                    $"    depfile = {self.DependencyFile.Value}"
                match self.ResponseFile with
                | Some (path, contents) ->
                    $"    rspfile = {path}"
                    $"    rspfile_content = {contents}"
                | None -> ()
            }

        statements |> String.concat "\n"

    static member def: Rule =
        { Name = ""
          Command = Array.empty
          Description = None
          DependencyFormat = DependencyFileFormat.def
          DependencyFile = None
          ResponseFile = None }

let escapeIfNeeded (x: string): string =
    if x.Contains ':' |> not then x else x.Replace(":", "$:")

type Build =
    { Rule: Rule
      Outputs: string []
      ImplicitOutputs: string []
      Inputs: string []
      ImplicitInputs: string []
      OrderOnlyInputs: string []
      Pool: string option
      Variables: Map<string, string> }
    static member def: Build =
        { Rule = Rule.def
          Outputs = Array.empty
          ImplicitOutputs = Array.empty
          Inputs = Array.empty
          ImplicitInputs = Array.empty
          OrderOnlyInputs = Array.empty
          Pool = None
          Variables = Map.empty }

    member self.Render(): string =
        let sb = StringBuilder()
        sb.Append("build ") |> ignore

        sb.AppendJoin(' ', self.Outputs |> Array.map escapeIfNeeded)
        |> ignore

        let emitArray (pfx: string) (ary: string []) =
            if 0 < ary.Length then
                sb.Append(pfx) |> ignore

                sb.AppendJoin(' ', ary |> Array.map escapeIfNeeded)
                |> ignore
            else
                ()

        if 0 < self.ImplicitOutputs.Length then self.ImplicitOutputs |> emitArray " | "

        sb.Append($" : {self.Rule.Name}") |> ignore
        self.Inputs |> emitArray " "

        if 0 < self.ImplicitInputs.Length then self.ImplicitInputs |> emitArray " | "

        if 0 < self.OrderOnlyInputs.Length then self.OrderOnlyInputs |> emitArray " || "

        match self.Pool with
        | None -> ()
        | Some x -> sb.Append($"\n    pool = {x}") |> ignore

        let s =
            query {
                for kv in self.Variables do
                    sortBy kv.Key
                    select $"    {kv.Key} = {kv.Value}"
            }

        sb.Append ('\n') |> ignore
        sb.AppendJoin('\n', s) |> ignore

        sb.ToString()
