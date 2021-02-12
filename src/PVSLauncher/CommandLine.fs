// Copyright (c) 2021 Masashi Fujita. All rights reserved.
module PVSLauncher.CommandLine

open FParsec

module OptionParser =
    type UserState =
        { quote: char option }
        static member Default: UserState = { quote = None }

    type Parser<'t> = Parser<'t, UserState>

    let internal quote (q: char) =
        let pq = pstring (string q)

        let inQuote =
            updateUserState (fun us -> { us with quote = Some q })

        let outQuote =
            updateUserState (fun us -> { us with quote = None })

        let isNotQuoted =
            userStateSatisfies (fun (us: UserState) ->
                match us.quote with
                | Some x when x = q -> false
                | _ -> true)

        isNotQuoted
        >>. between pq pq (between inQuote outQuote (many1SatisfyL (fun x -> x <> q) "quoted"))

    let internal isSpace (x: char) =
        match x with
        | ' '
        | '\t' -> true
        | _ -> false

    let internal str': Parser<_> = many1SatisfyL (isSpace >> not) "str"

    let internal str: Parser<_> =
        choiceL [ quote '\''; quote '"'; str' ] "str"

    let args: Parser<_> = sepEndBy1 str spaces |>> List.toArray

/// Splits supplied string.
let split (x: string): string [] =
    let r =
        runParserOnString OptionParser.args OptionParser.UserState.Default "split" x

    match r with
    | ParserResult.Failure (msg, _, _) -> failwithf "parse error: %A" msg
    | ParserResult.Success (s, _, _) -> s
