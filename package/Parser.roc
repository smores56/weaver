interface Parser
    exposes [parseArgs, Arg, ArgValue, ArgParseErr]
    imports []

Arg : [
    Short Str,
    ShortGroup { names : List Str, complete : [Complete, Partial] },
    Long { name : Str, value : Result Str [NoValue] },
    Parameter Str,
]

ArgValue : Result Str [NoValue]

ArgParseErr : [InvalidArg Str, MissingParameterValue Str]

parseArgs : List Str -> Result (List Arg) ArgParseErr
parseArgs = \args ->
    args
    |> List.dropFirst 1
    |> List.mapTry parseArg

parseArg : Str -> Result Arg ArgParseErr
parseArg = \arg ->
    when Str.splitFirst arg "-" is
        Ok { before: "", after } ->
            if after == "" then
                Ok (Parameter "-")
            else
                when Str.splitFirst after "-" is
                    Ok { before: "", after: rest } ->
                        if rest == "" then
                            Ok (Parameter "--")
                        else if Str.startsWith rest "-" then
                            Err (InvalidArg arg)
                        else
                            Ok (parseLongArg rest)

                    _other -> Ok (constructSetOfOptions after)

        _other ->
            Ok (Parameter arg)

parseLongArg : Str -> Arg
parseLongArg = \arg ->
    when Str.splitFirst arg "=" is
        Ok { before: option, after: value } ->
            Long { name: option, value: Ok value }

        _other ->
            Long { name: arg, value: Err NoValue }

constructSetOfOptions : Str -> Arg
constructSetOfOptions = \combined ->
    options =
        combined
        |> Str.toUtf8
        |> List.keepOks \c -> Str.fromUtf8 [c]

    when options is
        [alone] -> Short alone
        other -> ShortGroup { names: other, complete: Complete }

expect
    parsed = parseArg "-"

    parsed == Ok (Parameter "-")

expect
    parsed = parseArg "-a"

    parsed == Ok (Short "a")

expect
    parsed = parseArg "-abc"

    parsed == Ok (ShortGroup { names: ["a", "b", "c"], complete: Complete })

expect
    parsed = parseArg "--abc"

    parsed == Ok (Long { name: "abc", value: Err NoValue })

expect
    parsed = parseArg "--abc=xyz"

    parsed == Ok (Long { name: "abc", value: Ok "xyz" })

expect
    parsed = parseArg "123"

    parsed == Ok (Parameter "123")

expect
    parsed = parseArgs ["this-wont-show", "-a", "123", "--passed", "-bcd", "xyz", "--", "--subject=world"]

    parsed
    == Ok [
        Short "a",
        Parameter "123",
        Long { name: "passed", value: Err NoValue },
        ShortGroup { names: ["b", "c", "d"], complete: Complete },
        Parameter "xyz",
        Parameter "--",
        Long { name: "subject", value: Ok "world" },
    ]
