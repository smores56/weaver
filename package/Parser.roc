module [parse_args, ParsedArg, ArgValue]

import Arg exposing [Arg]

ArgValue : Result Arg [NoValue]

ParsedArg : [
    Short Str,
    ShortGroup { names : List Str, complete : [Complete, Partial] },
    Long { name : Str, value : ArgValue },
    Parameter Arg,
]

single_dash_arg = Arg.from_str("-")
double_dash_arg = Arg.from_str("--")

parse_args : List Arg -> List ParsedArg
parse_args = |args|
    starting_state = { parsed_args: [], pass_through: KeepParsing }

    state_after =
        args
        |> List.drop_first(1)
        |> List.walk(starting_state, |{ parsed_args, pass_through }, arg|
            when pass_through is
                KeepParsing ->
                    parsed_arg = parse_arg(arg)
                    when parsed_arg is
                        Parameter(a) if a == double_dash_arg ->
                            { pass_through: PassThrough, parsed_args }

                        _other ->
                            { pass_through: KeepParsing, parsed_args: parsed_args |> List.append(parsed_arg) }

                PassThrough ->
                    {
                        pass_through: PassThrough,
                        parsed_args: parsed_args |> List.append(Parameter(arg)),
                    })

    state_after.parsed_args

parse_arg : Arg -> ParsedArg
parse_arg = |arg|
    str_arg =
        when Arg.to_str(arg) is
            Ok(str) -> str
            Err(InvalidUtf8) ->
                return Parameter(arg)

    when Str.split_first(str_arg, "-") is
        Ok({ before: "", after }) ->
            if after == "" then
                Parameter(single_dash_arg)
            else
                when Str.split_first(after, "-") is
                    Ok({ before: "", after: rest }) ->
                        if rest == "" || Str.starts_with(rest, "-") then
                            Parameter(arg)
                        else
                            parse_long_arg(rest)

                    _other -> construct_set_of_options(after)

        _other ->
            Parameter(arg)

parse_long_arg : Str -> ParsedArg
parse_long_arg = |arg|
    when Str.split_first(arg, "=") is
        Ok({ before: option, after: value }) ->
            Long({ name: option, value: Ok(Arg.from_str(value)) })

        _other ->
            Long({ name: arg, value: Err(NoValue) })

construct_set_of_options : Str -> ParsedArg
construct_set_of_options = |combined|
    options =
        combined
        |> Str.to_utf8
        |> List.keep_oks(|c| Str.from_utf8([c]))

    when options is
        [alone] -> Short(alone)
        other -> ShortGroup({ names: other, complete: Complete })

expect
    parsed = parse_arg(Arg.from_str("-"))

    parsed == Parameter(Arg.from_str("-"))

expect
    parsed = parse_arg(Arg.from_str("-a"))

    parsed == Short("a")

expect
    parsed = parse_arg(Arg.from_str("-abc"))

    parsed == ShortGroup({ names: ["a", "b", "c"], complete: Complete })

expect
    parsed = parse_arg(Arg.from_str("--abc"))

    parsed == Long({ name: "abc", value: Err(NoValue) })

expect
    parsed = parse_arg(Arg.from_str("--abc=xyz"))

    parsed == Long({ name: "abc", value: Ok(Arg.from_str("xyz")) })

expect
    parsed = parse_arg(Arg.from_str("123"))

    parsed == Parameter(Arg.from_str("123"))

expect
    parsed =
        ["this-wont-show", "-a", "123", "--passed", "-bcd", "xyz", "--", "--subject=world"]
        |> List.map(Arg.from_str)
        |> parse_args

    parsed
    == [
        Short("a"),
        Parameter(Arg.from_str("123")),
        Long({ name: "passed", value: Err(NoValue) }),
        ShortGroup({ names: ["b", "c", "d"], complete: Complete }),
        Parameter(Arg.from_str("xyz")),
        Parameter(Arg.from_str("--subject=world")),
    ]
