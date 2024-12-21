module [parse_args, Arg, ArgValue]

Arg : [
    Short Str,
    ShortGroup { names : List Str, complete : [Complete, Partial] },
    Long { name : Str, value : Result Str [NoValue] },
    Parameter Str,
]

ArgValue : Result Str [NoValue]

parse_args : List Str -> List Arg
parse_args = \args ->
    starting_state = { parsed_args: [], pass_through: KeepParsing }

    state_after =
        args
        |> List.dropFirst 1
        |> List.walk starting_state \{ parsed_args, pass_through }, arg ->
            when pass_through is
                KeepParsing ->
                    parsed_arg = parse_arg arg
                    when parsed_arg is
                        Parameter "--" ->
                            { pass_through: PassThrough, parsed_args }

                        _other ->
                            { pass_through: KeepParsing, parsed_args: parsed_args |> List.append parsed_arg }

                PassThrough ->
                    {
                        pass_through: PassThrough,
                        parsed_args: parsed_args |> List.append (Parameter arg),
                    }

    state_after.parsed_args

parse_arg : Str -> Arg
parse_arg = \arg ->
    when Str.splitFirst arg "-" is
        Ok { before: "", after } ->
            if after == "" then
                Parameter "-"
            else
                when Str.splitFirst after "-" is
                    Ok { before: "", after: rest } ->
                        if rest == "" || Str.startsWith rest "-" then
                            Parameter arg
                        else
                            parse_long_arg rest

                    _other -> construct_set_of_options after

        _other ->
            Parameter arg

parse_long_arg : Str -> Arg
parse_long_arg = \arg ->
    when Str.splitFirst arg "=" is
        Ok { before: option, after: value } ->
            Long { name: option, value: Ok value }

        _other ->
            Long { name: arg, value: Err NoValue }

construct_set_of_options : Str -> Arg
construct_set_of_options = \combined ->
    options =
        combined
        |> Str.toUtf8
        |> List.keepOks \c -> Str.fromUtf8 [c]

    when options is
        [alone] -> Short alone
        other -> ShortGroup { names: other, complete: Complete }

expect
    parsed = parse_arg "-"

    parsed == Parameter "-"

expect
    parsed = parse_arg "-a"

    parsed == Short "a"

expect
    parsed = parse_arg "-abc"

    parsed == ShortGroup { names: ["a", "b", "c"], complete: Complete }

expect
    parsed = parse_arg "--abc"

    parsed == Long { name: "abc", value: Err NoValue }

expect
    parsed = parse_arg "--abc=xyz"

    parsed == Long { name: "abc", value: Ok "xyz" }

expect
    parsed = parse_arg "123"

    parsed == Parameter "123"

expect
    parsed = parse_args ["this-wont-show", "-a", "123", "--passed", "-bcd", "xyz", "--", "--subject=world"]

    parsed
    == [
        Short "a",
        Parameter "123",
        Long { name: "passed", value: Err NoValue },
        ShortGroup { names: ["b", "c", "d"], complete: Complete },
        Parameter "xyz",
        Parameter "--subject=world",
    ]
