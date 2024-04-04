interface Parser
    exposes [parseArgs, Arg, ArgParseErr]
    imports []

Arg : [
    Short { name : Str, grouped : [Alone, Grouped] },
    Long { name : Str, value : Result Str [NoValue] },
    Parameter Str,
]

ArgParseErr : [InvalidArg Str, MissingParameterValue Str]

parseArgs : List Str -> Result (List Arg) ArgParseErr
parseArgs = \args ->
    args
    |> List.dropFirst 1
    |> List.mapTry parseArg
    |> Result.map List.join

parseArg : Str -> Result (List Arg) ArgParseErr
parseArg = \arg ->
    when Str.splitFirst arg "-" is
        Ok { before: "", after } ->
            if after == "" then
                Ok [Parameter "-"]
            else
                when Str.splitFirst after "-" is
                    Ok { before: "", after: rest } ->
                        if rest == "" then
                            Ok [Parameter "--"]
                        else if Str.startsWith rest "-" then
                            Err (InvalidArg arg)
                        else
                            Ok [parseLongArg rest]

                    _other -> Ok (constructSetOfOptions after)

        _other ->
            Ok [Parameter arg]

parseLongArg : Str -> Arg
parseLongArg = \arg ->
    when Str.splitFirst arg "=" is
        Ok { before: option, after: value } ->
            Long { name: option, value: Ok value }

        _other ->
            Long { name: arg, value: Err NoValue }

constructSetOfOptions : Str -> List Arg
constructSetOfOptions = \combined ->
    options =
        combined
        |> Str.toUtf8
        |> List.keepOks \c -> Str.fromUtf8 [c]

    when options is
        [alone] -> [Short { name: alone, grouped: Alone }]
        _other ->
            List.map options \name ->
                Short { name, grouped: Grouped }

expect
    parsed = parseArg "-"

    parsed == Ok [Parameter "-"]

expect
    parsed = parseArg "-a"

    parsed == Ok [Short { name: "a", grouped: Alone }]

expect
    parsed = parseArg "-abc"

    parsed
    == Ok [
        Short { name: "a", grouped: Grouped },
        Short { name: "b", grouped: Grouped },
        Short { name: "c", grouped: Grouped },
    ]

expect
    parsed = parseArg "--abc"

    parsed == Ok [Long { name: "abc", value: Err NoValue }]

expect
    parsed = parseArg "--abc=xyz"

    parsed == Ok [Long { name: "abc", value: Ok "xyz" }]

expect
    parsed = parseArg "123"

    parsed == Ok [Parameter "123"]

expect
    parsed = parseArgs ["this-wont-show", "-a", "123", "--passed", "-bcd", "xyz", "--", "--subject=world"]

    parsed
    == Ok [
        Short { name: "a", grouped: Alone },
        Parameter "123",
        Long { name: "passed", value: Err NoValue },
        Short { name: "b", grouped: Grouped },
        Short { name: "c", grouped: Grouped },
        Short { name: "d", grouped: Grouped },
        Parameter "xyz",
        Parameter "--",
        Long { name: "subject", value: Ok "world" },
    ]
