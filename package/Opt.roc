## Options that your CLI will parse as fields in your config.
module [
    single,
    maybe,
    list,
    flag,
    count,
    arg,
    maybe_arg,
    arg_list,
    bytes,
    maybe_bytes,
    bytes_list,
    str,
    maybe_str,
    str_list,
    dec,
    maybe_dec,
    dec_list,
    f32,
    maybe_f32,
    f32_list,
    f64,
    maybe_f64,
    f64_list,
    u8,
    maybe_u8,
    u8_list,
    u16,
    maybe_u16,
    u16_list,
    u32,
    maybe_u32,
    u32_list,
    u64,
    maybe_u64,
    u64_list,
    u128,
    maybe_u128,
    u128_list,
    i8,
    maybe_i8,
    i8_list,
    i16,
    maybe_i16,
    i16_list,
    i32,
    maybe_i32,
    i32_list,
    i64,
    maybe_i64,
    i64_list,
    i128,
    maybe_i128,
    i128_list,
]

import Arg exposing [Arg]
import Builder exposing [CliBuilder, GetOptionsAction]
import Base exposing [
    ArgExtractErr,
    OptionConfigBaseParams,
    DefaultableOptionConfigBaseParams,
    OptionConfigParams,
    DefaultableOptionConfigParams,
    OptionConfig,
    str_type_name,
    num_type_name,
]
import Extract exposing [extract_option_values]
import Parser exposing [ArgValue]

builder_with_option_parser : OptionConfig, (List ArgValue -> Result data ArgExtractErr) -> CliBuilder data from_action to_action
builder_with_option_parser = |option, value_parser|
    arg_parser = |args|
        { values, remaining_args } = extract_option_values({ args, option })?
        data = value_parser(values)?

        Ok({ data, remaining_args })

    Builder.from_arg_parser(arg_parser)
    |> Builder.add_option(option)

get_maybe_value : List ArgValue, OptionConfig -> Result (Result ArgValue [NoValue]) ArgExtractErr
get_maybe_value = |values, option|
    when values is
        [] -> Ok(Err(NoValue))
        [single_value] -> Ok(Ok(single_value))
        [..] -> Err(OptionCanOnlyBeSetOnce(option))

## Add a required option that takes a custom type to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err(InvalidValue(Str))`
## on failure, where the `Str` is the reason the parsing failed that will
## get displayed in the incorrect usage message.
##
## Parsing arguments will fail if the option is not given as an argument
## or a value is not provided to the option.
##
## ```roc
## expect
##     Color : [Green, Red, Blue]
##
##     parse_color : Arg -> Result Color [InvalidValue Str, InvalidUtf8]
##     parse_color = |color|
##         when Arg.to_str(color) is
##             Ok("green") -> Ok(Green)
##             Ok("red") -> Ok(Red)
##             Ok("blue") -> Ok(Blue)
##             other -> Err(InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Opt.single({ short: "c", parser: parse_color, type: "color" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "-c", "green"])
##     == SuccessfullyParsed(Green)
## ```
single : DefaultableOptionConfigParams a -> CliBuilder a GetOptionsAction GetOptionsAction
single = |{ parser, type, short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    option = { expected_value: ExpectsValue(type), plurality: One, short, long, help }

    default_generator = |{}|
        when default is
            Value(default_value) -> Ok(default_value)
            Generate(generator) -> Ok(generator({}))
            NoDefault -> Err(MissingOption(option))

    value_parser = |values|
        value = get_maybe_value(values, option)?

        when value is
            Err(NoValue) -> default_generator({})
            Ok(Err(NoValue)) -> Err(NoValueProvidedForOption(option))
            Ok(Ok(val)) ->
                parser(val)
                |> Result.map_err(|err| InvalidOptionValue(err, option))

    builder_with_option_parser(option, value_parser)

## Add an optional option that takes a custom type to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err(InvalidValue(Str))`
## on failure, where the `Str` is the reason the parsing failed that will
## get displayed in the incorrect usage message.
##
## Parsing arguments will fail if more than one instance of the argument
## is provided, there is no value given for the option call, or the value
## doesn't parse correctly.
##
## ```roc
## expect
##     Color : [Green, Red, Blue]
##
##     parse_color : Arg -> Result Color [InvalidValue Str, InvalidUtf8]
##     parse_color = |color|
##         when Arg.to_str(color) is
##             Ok("green") -> Ok(Green)
##             Ok("red") -> Ok(Red)
##             Ok("blue") -> Ok(Blue)
##             other -> Err(InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Opt.maybe({ short: "c", type: "color", parser: parse_color })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe : OptionConfigParams data -> CliBuilder (Result data [NoValue]) GetOptionsAction GetOptionsAction
maybe = |{ parser, type, short ?? "", long ?? "", help ?? "" }|
    option = { expected_value: ExpectsValue(type), plurality: Optional, short, long, help }

    value_parser = |values|
        value = get_maybe_value(values, option)?

        when value is
            Err(NoValue) -> Ok(Err(NoValue))
            Ok(Err(NoValue)) -> Err(NoValueProvidedForOption(option))
            Ok(Ok(val)) ->
                parser(val)
                |> Result.map_ok(Ok)
                |> Result.map_err(|err| InvalidOptionValue(err, option))

    builder_with_option_parser(option, value_parser)

## Add an option that takes a custom type and can be given multiple times
## to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err(InvalidValue(Str))`
## on failure, where the `Str` is the reason the parsing failed that will
## get displayed in the incorrect usage message.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value or any of the options don't parse correctly.
##
## ```roc
## expect
##     Color : [Green, Red, Blue]
##
##     parse_color : Arg -> Result Color [InvalidValue Str, InvalidUtf8]
##     parse_color = |color|
##         when Arg.to_str(color) is
##             Ok("green") -> Ok(Green)
##             Ok("red") -> Ok(Red)
##             Ok("blue") -> Ok(Blue)
##             other -> Err(InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Opt.list({ short: "c", type: "color", parser: parse_color })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "-c", "green", "--color=red"])
##     == SuccessfullyParsed([Green, Red])
## ```
list : OptionConfigParams data -> CliBuilder (List data) GetOptionsAction GetOptionsAction
list = |{ parser, type, short ?? "", long ?? "", help ?? "" }|
    option = { expected_value: ExpectsValue(type), plurality: Many, short, long, help }

    value_parser = |values|
        List.map_try(values, |value|
            when value is
                Err(NoValue) -> Err(NoValueProvidedForOption(option))
                Ok(val) ->
                    parser(val)
                    |> Result.map_err(|err| InvalidOptionValue(err, option)))

    builder_with_option_parser(option, value_parser)

## Add an optional flag to your CLI builder.
##
## Parsing arguments will fail if the flag is given more than once
## or if a value is provided to it, e.g. `--flag=value`.
##
## ```roc
## expect
##     { parser } =
##         Opt.flag({ short: "f", long: "force" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "-f"])
##     == SuccessfullyParsed(Bool.true)
## ```
flag : OptionConfigBaseParams -> CliBuilder Bool GetOptionsAction GetOptionsAction
flag = |{ short ?? "", long ?? "", help ?? "" }|
    option = { expected_value: NothingExpected, plurality: Optional, short, long, help }

    value_parser = |values|
        value = get_maybe_value(values, option)?

        when value is
            Err(NoValue) -> Ok(Bool.false)
            Ok(Err(NoValue)) -> Ok(Bool.true)
            Ok(Ok(_val)) -> Err(OptionDoesNotExpectValue(option))

    builder_with_option_parser(option, value_parser)

## Add a flag that can be given multiple times to your CLI builder.
##
## Parsing arguments will fail if this flag is ever given a value,
## e.g. `--flag=value`.
##
## ```roc
## expect
##     { parser } =
##         Opt.count({ short: "f", long: "force" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "-f", "--force", "-fff"])
##     == SuccessfullyParsed(5)
## ```
count : OptionConfigBaseParams -> CliBuilder U64 GetOptionsAction GetOptionsAction
count = |{ short ?? "", long ?? "", help ?? "" }|
    option = { expected_value: NothingExpected, plurality: Many, short, long, help }

    value_parser = |values|
        if values |> List.any(Result.is_ok) then
            Err(OptionDoesNotExpectValue(option))
        else
            Ok(List.len(values))

    builder_with_option_parser(option, value_parser)

## Add a required option that takes an [Arg] to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument
## or a value is not provided to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.arg({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=abc"])
##     == SuccessfullyParsed(Arg.from_str("abc"))
## ```
arg : DefaultableOptionConfigBaseParams Arg -> CliBuilder Arg GetOptionsAction GetOptionsAction
arg = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: Ok,
        type: str_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes an [Arg] to your CLI builder.
##
## Parsing arguments will fail if more than one instance of the argument
## is provided or there is no value given for the option call.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_arg({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_arg : OptionConfigBaseParams -> CliBuilder (Result Arg [NoValue]) GetOptionsAction GetOptionsAction
maybe_arg = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: Ok,
        type: str_type_name,
        short,
        long,
        help,
    })

## Add an option that takes an [Arg] and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value.
##
## ```roc
## expect
##     { parser } =
##         Opt.arg_list({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "abc", "--answer", "def", "--answer=ghi"])
##     == SuccessfullyParsed(List.map(["abc", "def", "ghi"], Arg.from_str))
## ```
arg_list : OptionConfigBaseParams -> CliBuilder (List Arg) GetOptionsAction GetOptionsAction
arg_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: Ok,
        type: str_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a byte list to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument
## or a value is not provided to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.bytes({ long: "answer" })
##         |> Cli.finish({ name: "exampl)e" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=abc"])
##     == SuccessfullyParsed([97, 98, 99])
## ```
bytes : DefaultableOptionConfigBaseParams (List U8) -> CliBuilder (List U8) GetOptionsAction GetOptionsAction
bytes = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Ok(Arg.to_bytes(a)),
        type: str_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a byte list to your CLI builder.
##
## Parsing arguments will fail if more than one instance of the argument
## is provided or there is no value given for the option call.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_bytes({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_bytes : OptionConfigBaseParams -> CliBuilder (Result (List U8) [NoValue]) GetOptionsAction GetOptionsAction
maybe_bytes = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Ok(Arg.to_bytes(a)),
        type: str_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a byte list and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value.
##
## ```roc
## expect
##     { parser } =
##         Opt.bytes_list({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "abc", "--answer", "def", "--answer=ghi"])
##     == SuccessfullyParsed([[97, 98, 99], [100, 101, 102], [103, 104, 105]])
## ```
bytes_list : OptionConfigBaseParams -> CliBuilder (List (List U8)) GetOptionsAction GetOptionsAction
bytes_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Ok(Arg.to_bytes(a)),
        type: str_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a string to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not valid UTF-8.
##
## ```roc
## expect
##     { parser } =
##         Opt.str({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=abc"])
##     == SuccessfullyParsed("abc")
## ```
str : DefaultableOptionConfigBaseParams Str -> CliBuilder Str GetOptionsAction GetOptionsAction
str = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: Arg.to_str,
        type: str_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a string to your CLI builder.
##
## Parsing arguments will fail if more than one instance of the argument
## is provided, there is no value given for the option call, or the value
## is not valid UTF-8.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_str({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_str : OptionConfigBaseParams -> CliBuilder (Result Str [NoValue]) GetOptionsAction GetOptionsAction
maybe_str = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: Arg.to_str,
        type: str_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a string and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value or any of the values are not valid UTF-8.
##
## ```roc
## expect
##     { parser } =
##         Opt.str_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "abc", "--answer", "def", "--answer=ghi"])
##     == SuccessfullyParsed(["abc", "def", "ghi"])
## ```
str_list : OptionConfigBaseParams -> CliBuilder (List Str) GetOptionsAction GetOptionsAction
str_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: Arg.to_str,
        type: str_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a `Dec` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.dec({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42.5"])
##     == SuccessfullyParsed(2.5)
## ```
dec : DefaultableOptionConfigBaseParams Dec -> CliBuilder Dec GetOptionsAction GetOptionsAction
dec = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_dec),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a `Dec` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_dec({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_dec : OptionConfigBaseParams -> CliBuilder (Result Dec [NoValue]) GetOptionsAction GetOptionsAction
maybe_dec = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_dec),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a `Dec` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.dec_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "-3.0"])
##     == SuccessfullyParsed([1.0, 2.0, -3.0])
## ```
dec_list : OptionConfigBaseParams -> CliBuilder (List Dec) GetOptionsAction GetOptionsAction
dec_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_dec),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a `F32` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.f32({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42.5"])
##     == SuccessfullyParsed(2.5)
## ```
f32 : DefaultableOptionConfigBaseParams F32 -> CliBuilder F32 GetOptionsAction GetOptionsAction
f32 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_f32),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a `F32` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_f32({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_f32 : OptionConfigBaseParams -> CliBuilder (Result F32 [NoValue]) GetOptionsAction GetOptionsAction
maybe_f32 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_f32),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a `F32` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.f32_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "-3.0"])
##     == SuccessfullyParsed([1.0, 2.0, -3.0])
## ```
f32_list : OptionConfigBaseParams -> CliBuilder (List F32) GetOptionsAction GetOptionsAction
f32_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_f32),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a `F64` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.f64({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42.5"])
##     == SuccessfullyParsed(2.5)
## ```
f64 : DefaultableOptionConfigBaseParams F64 -> CliBuilder F64 GetOptionsAction GetOptionsAction
f64 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_f64),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a `F64` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_f64({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_f64 : OptionConfigBaseParams -> CliBuilder (Result F64 [NoValue]) GetOptionsAction GetOptionsAction
maybe_f64 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_f64),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a `F64` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.f64_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "-3.0"])
##     == SuccessfullyParsed([1.0, 2.0, -3.0])
## ```
f64_list : OptionConfigBaseParams -> CliBuilder (List F64) GetOptionsAction GetOptionsAction
f64_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_f64),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a `U8` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u8({ long: "answer" })
##         |> Cli.finish({ name: "exa)mple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
u8 : DefaultableOptionConfigBaseParams U8 -> CliBuilder U8 GetOptionsAction GetOptionsAction
u8 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u8),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a `U8` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_u8({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u8 : OptionConfigBaseParams -> CliBuilder (Result U8 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u8 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u8),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a `U8` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u8_list({ long: "answer" })
##         |> Cli.finish({ name: "example") }
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
u8_list : OptionConfigBaseParams -> CliBuilder (List U8) GetOptionsAction GetOptionsAction
u8_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u8),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a `U16` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u16({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
u16 : DefaultableOptionConfigBaseParams U16 -> CliBuilder U16 GetOptionsAction GetOptionsAction
u16 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u16),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a `U16` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_u16({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u16 : OptionConfigBaseParams -> CliBuilder (Result U16 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u16 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u16),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a `U16` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u16_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
u16_list : OptionConfigBaseParams -> CliBuilder (List U16) GetOptionsAction GetOptionsAction
u16_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u16),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a `U32` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u32({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
u32 : DefaultableOptionConfigBaseParams U32 -> CliBuilder U32 GetOptionsAction GetOptionsAction
u32 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u32),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a `U32` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_u32({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u32 : OptionConfigBaseParams -> CliBuilder (Result U32 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u32 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u32),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a `U32` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u32_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
u32_list : OptionConfigBaseParams -> CliBuilder (List U32) GetOptionsAction GetOptionsAction
u32_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u32),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a `U64` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u64({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
u64 : DefaultableOptionConfigBaseParams U64 -> CliBuilder U64 GetOptionsAction GetOptionsAction
u64 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u64),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a `U64` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_u64({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u64 : OptionConfigBaseParams -> CliBuilder (Result U64 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u64 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u64),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a `U64` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u64_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
u64_list : OptionConfigBaseParams -> CliBuilder (List U64) GetOptionsAction GetOptionsAction
u64_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u64),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes a `U128` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u128({ long: "answer" })
##         |> Cli.finish({ name: "examp)le" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
u128 : DefaultableOptionConfigBaseParams U128 -> CliBuilder U128 GetOptionsAction GetOptionsAction
u128 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u128),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes a `U128` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_u128({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u128 : OptionConfigBaseParams -> CliBuilder (Result U128 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u128 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u128),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes a `U128` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u128_list({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
u128_list : OptionConfigBaseParams -> CliBuilder (List U128) GetOptionsAction GetOptionsAction
u128_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_u128),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes an `I8` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i8({ long: "answer" })
##         |> Cli.finish({ name: "exa)mple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
i8 : DefaultableOptionConfigBaseParams I8 -> CliBuilder I8 GetOptionsAction GetOptionsAction
i8 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i8),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes an `I8` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_i8({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i8 : OptionConfigBaseParams -> CliBuilder (Result I8 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i8 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i8),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes an `I8` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i8_list({ long: "answer" })
##         |> Cli.finish({ name: "example") }
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
i8_list : OptionConfigBaseParams -> CliBuilder (List I8) GetOptionsAction GetOptionsAction
i8_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i8),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes an `I16` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i16({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
i16 : DefaultableOptionConfigBaseParams I16 -> CliBuilder I16 GetOptionsAction GetOptionsAction
i16 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i16),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes an `I16` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_i16({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i16 : OptionConfigBaseParams -> CliBuilder (Result I16 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i16 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i16),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes an `I16` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i16_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
i16_list : OptionConfigBaseParams -> CliBuilder (List I16) GetOptionsAction GetOptionsAction
i16_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i16),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes an `I32` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i32({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
i32 : DefaultableOptionConfigBaseParams I32 -> CliBuilder I32 GetOptionsAction GetOptionsAction
i32 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i32),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes an `I32` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_i32({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i32 : OptionConfigBaseParams -> CliBuilder (Result I32 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i32 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i32),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes an `I32` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i32_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
i32_list : OptionConfigBaseParams -> CliBuilder (List I32) GetOptionsAction GetOptionsAction
i32_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i32),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes an `I64` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i64({ long: "answer" })
##         |> Cli.finish({ name: "exam)ple" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
i64 : DefaultableOptionConfigBaseParams I64 -> CliBuilder I64 GetOptionsAction GetOptionsAction
i64 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i64),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes an `I64` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_i64({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i64 : OptionConfigBaseParams -> CliBuilder (Result I64 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i64 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i64),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes an `I64` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i64_list({ long: "answer" })
##         |> Cli.finish({ name: "example" )}
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
i64_list : OptionConfigBaseParams -> CliBuilder (List I64) GetOptionsAction GetOptionsAction
i64_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i64),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add a required option that takes an `I128` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i128({ long: "answer" })
##         |> Cli.finish({ name: "examp)le" }
##         |> Cli.assert_valid
##
##     parser(["example", "--answer=42"])
##     == SuccessfullyParsed(2)
## ```
i128 : DefaultableOptionConfigBaseParams I128 -> CliBuilder I128 GetOptionsAction GetOptionsAction
i128 = |{ short ?? "", long ?? "", help ?? "", default ?? NoDefault }|
    single({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i128),
        type: num_type_name,
        short,
        long,
        help,
        default,
    })

## Add an optional option that takes an `I128` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybe_i128({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i128 : OptionConfigBaseParams -> CliBuilder (Result I128 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i128 = |{ short ?? "", long ?? "", help ?? "" }|
    maybe({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i128),
        type: num_type_name,
        short,
        long,
        help,
    })

## Add an option that takes an `I128` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i128_list({ long: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "-a", "1", "--answer=2", "--answer", "3"])
##     == SuccessfullyParsed([1, 2, 3])
## ```
i128_list : OptionConfigBaseParams -> CliBuilder (List I128) GetOptionsAction GetOptionsAction
i128_list = |{ short ?? "", long ?? "", help ?? "" }|
    list({
        parser: |a| Arg.to_str(a) |> Result.try(Str.to_i128),
        type: num_type_name,
        short,
        long,
        help,
    })
