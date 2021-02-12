namespace PVSLauncher.Tests

open Expecto

module Ninja =
    open PVSLanucher.Ninja

    [<Tests>]
    let testRule =
        testList
            "test rule generation"
            [ testCase "default" (fun _ ->
                  let defaultRule = { Rule.def with Name = "TEST" }

                  let expected = """
rule TEST
    command = """

                  let actual = defaultRule.Render()
                  Expect.equal actual (expected.TrimStart()) "should equal")
              testCase "full" (fun _ ->
                  let rule: Rule =
                      { Name = "FULL"
                        Command = [| "abc"; "def"; "ghi" |]
                        Description = "FULL description" |> Some
                        DependencyFormat = DependencyFileFormat.MSVC
                        DependencyFile = "deps.d" |> Some
                        ResponseFile = ("out.rsp", "$in_newline") |> Some }

                  let expected = """
rule FULL
    description = FULL description
    command = abc def ghi
    deps = msvc
    depfile = deps.d
    rspfile = out.rsp
    rspfile_content = $in_newline"""
                  let actual = rule.Render()
                  Expect.equal actual (expected.TrimStart()) "should equal") ]

    [<Tests>]
    let testBuild =
        testList
            "test build command generation"
            [ testCase "default" (fun _ ->
                  let rule = { Rule.def with Name = "BUILD" }
                  let build = { Build.def with Rule = rule }

                  let expected = """
build  : BUILD
"""
                  let actual = build.Render()
                  Expect.equal actual (expected.TrimStart()) "should equal")
              testCase "full" (fun _ ->
                  let rule = { Rule.def with Name = "BUILD" }

                  let build =
                      { Build.def with
                            Rule = rule
                            Outputs = [| "abc"; "def"; "ghi" |]
                            ImplicitOutputs = [| "jkl"; "mno"; "pqr" |]
                            Inputs = [| "ABC"; "DEF"; "GHI" |]
                            ImplicitInputs = [| "JKL"; "MNO"; "PQR" |]
                            OrderOnlyInputs = [| "STU"; "VWX" |]
                            Pool = Some "POOL"
                            Variables = [ ("c", "d"); ("a", "b") ] |> Map.ofList }

                  let expected = """
build abc def ghi | jkl mno pqr : BUILD ABC DEF GHI | JKL MNO PQR || STU VWX
    pool = POOL
    a = b
    c = d"""
                  let actual = build.Render()
                  Expect.equal actual (expected.TrimStart()) "should equal") ]
