namespace PVSLauncher.Tests

open Expecto

module CommandLine =
    open FParsec
    open PVSLauncher.CommandLine.OptionParser

    let srcArgs =
        [| "clang.exe"
           "-DPDISTD_ENABLE_FORMAT_STRING_VALIDATION=1"
           "-O3"
           "-DNDEBUG"
           "-O3"
           "-DNDEBUG"
           "-std=c++14"
           "-frtti"
           "-fexceptions"
           "-ginlined-scopes"
           "-fdiagnostics-format=msvc"
           "-mavx"
           "-g"
           "-O2"
           "-DNDEBUG"
           "-DCOMPILE_BUILD_RELEASE=1"
           "-DCOMPILE_USER_BUILD=1"
           "-Wall"
           "-Wno-unused-private-field"
           "-Wno-gnu"
           "-Wno-invalid-offsetof"
           "-Wno-invalid-constexpr"
           "-Wno-multichar"
           "-Wno-parentheses-equality"
           "-Wno-error=dynamic-exception-spec"
           "-Wno-error=#warning"
           "-Werror=dangling"
           "-o"
           "foo.cxx.obj"
           "-c"
           "-Q"
           "foo.cxx" |]

    [<Tests>]
    let parserTest =
        testList
            "parser tests"
            [ testCase "parse single string"
              <| fun _ ->
                  let r =
                      runParserOnString args UserState.Default "test input" "abc"

                  match r with
                  | ParserResult.Failure (msg, _, _) -> failwithf "error: %A" msg
                  | ParserResult.Success (s, _, _) -> Expect.equal s [| "abc" |] "should be equal"
              testCase "parse quoted string"
              <| fun _ ->
                  let r =
                      runParserOnString args UserState.Default "test input" "\"abc\""

                  match r with
                  | ParserResult.Failure (msg, _, _) -> failwithf "error: %A" msg
                  | ParserResult.Success (s, _, _) -> Expect.equal s [| "abc" |] "should be equal"
              testCase "parse mixed"
              <| fun _ ->
                  let r =
                      runParserOnString args UserState.Default "test input" "abc \"d \\e f\""

                  match r with
                  | ParserResult.Failure (msg, _, _) -> failwithf "error: %A" msg
                  | ParserResult.Success (s, _, _) -> Expect.equal s [| "abc"; "d \\e f" |] "should be equal"
              testCase "parse real data"
              <| fun _ ->

                  let r =
                      runParserOnString args UserState.Default "realworld test input" (String.concat " " srcArgs)

                  match r with
                  | ParserResult.Failure (msg, _, _) -> failwithf "error: %A" msg
                  | ParserResult.Success (s, _, _) ->
                      Expect.equal
                          s.[0]
                          ("clang.exe")
                          "should equal"

                      //                      Expect.equal s.[9] (Option("O", Some("3"))) "should equal"
//                      Expect.equal s.[10] (Option("D", Some ("NDEBUG"))) "should equal"
                      printfn "# %A" s ]

    [<Tests>]
    let tests =
        testList
            "Test commandline parser"
            [ testCase "command only"
              <| fun _ ->
                  let cmd =
                      PVSLauncher.CommandLine.split "abc \"def' ghi\" jkl "

                  Expect.equal cmd [| "abc"; "def' ghi"; "jkl" |] "should match" ]
