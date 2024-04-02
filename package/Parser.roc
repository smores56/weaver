interface Parser
    exposes [parseArgs, Arg, ArgParseErr]
    imports []

Arg : [
    Short { name : Str, ordering : [NotLast, Last] },
    Long { name : Str, value : [NoValue, HasValue Str] },
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
            Long { name: option, value: HasValue value }

        _other ->
            Long { name: arg, value: NoValue }

constructSetOfOptions : Str -> List Arg
constructSetOfOptions = \combined ->
    options =
        combined
        |> Str.toUtf8
        |> List.keepOks \c -> Str.fromUtf8 [c]

    when options is
        [] -> []
        [.. as others, last] ->
            others
            |> List.map \other -> Short { name: other, ordering: NotLast }
            |> List.append (Short { name: last, ordering: Last })

expect
    parsed = parseArg "-"

    parsed == Ok [Parameter "-"]

expect
    parsed = parseArg "-a"

    parsed == Ok [Short { name: "a", ordering: Last }]

expect
    parsed = parseArg "-abc"

    parsed
    == Ok [
        Short { name: "a", ordering: NotLast },
        Short { name: "b", ordering: NotLast },
        Short { name: "c", ordering: Last },
    ]

expect
    parsed = parseArg "--abc"

    parsed == Ok [Long { name: "abc", value: NoValue }]

expect
    parsed = parseArg "--abc=xyz"

    parsed == Ok [Long { name: "abc", value: HasValue "xyz" }]

expect
    parsed = parseArg "123"

    parsed == Ok [Parameter "123"]

expect
    parsed = parseArgs ["this-won't-show", "-a", "123", "-bcd", "xyz", "--", "--subject=world"]

    parsed
    == Ok [
        Short { name: "a", ordering: Last },
        Parameter "123",
        Short { name: "b", ordering: NotLast },
        Short { name: "c", ordering: NotLast },
        Short { name: "d", ordering: Last },
        Parameter "xyz",
        Parameter "--",
        Long { name: "subject", value: HasValue "world" },
    ]
