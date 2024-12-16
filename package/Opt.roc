## Options that your CLI will parse as fields in your config.
module [
    single,
    maybe,
    list,
    flag,
    count,
    str,
    maybeStr,
    strList,
    dec,
    maybeDec,
    decList,
    f32,
    maybeF32,
    f32List,
    f64,
    maybeF64,
    f64List,
    u8,
    maybeU8,
    u8List,
    u16,
    maybeU16,
    u16List,
    u32,
    maybeU32,
    u32List,
    u64,
    maybeU64,
    u64List,
    u128,
    maybeU128,
    u128List,
    i8,
    maybeI8,
    i8List,
    i16,
    maybeI16,
    i16List,
    i32,
    maybeI32,
    i32List,
    i64,
    maybeI64,
    i64List,
    i128,
    maybeI128,
    i128List,
]

import Builder exposing [CliBuilder, GetOptionsAction]
import Base exposing [
    ArgExtractErr,
    OptionConfigBaseParams,
    DefaultableOptionConfigBaseParams,
    OptionConfigParams,
    DefaultableOptionConfigParams,
    OptionConfig,
    strTypeName,
    numTypeName,
]
import Extract exposing [extractOptionValues]
import Parser exposing [ArgValue]

builder_with_option_parser : OptionConfig, (List ArgValue -> Result data ArgExtractErr) -> CliBuilder data fromAction toAction
builder_with_option_parser = \option, value_parser ->
    arg_parser = \args ->
        { values, remaining_args } = try extractOptionValues { args, option }
        data = try valueParser values

        Ok { data, remainingArgs }

    Builder.fromArgParser argParser
    |> Builder.addOption option

get_maybe_value : List ArgValue, OptionConfig -> Result (Result ArgValue [NoValue]) ArgExtractErr
get_maybe_value = \values, option ->
    when values is
        [] -> Ok (Err NoValue)
        [single_value] -> Ok (Ok singleValue)
        [..] -> Err (OptionCanOnlyBeSetOnce option)

## Add a required option that takes a custom type to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err (InvalidValue Str)`
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
##     parseColor : Str -> Result Color [InvalidValue Str]
##     parseColor = \color ->
##         when color is
##             "green" -> Ok Green
##             "red" -> Ok Red
##             "blue" -> Ok Blue
##             other -> Err (InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Opt.single { short: "c", parser: parseColor, type: "color" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-c", "green"]
##     == SuccessfullyParsed Green
## ```
single : DefaultableOptionConfigParams a -> CliBuilder a GetOptionsAction GetOptionsAction
single = \{ parser, type, short ? "", long ? "", help ? "", default ? NoDefault } ->
    option = { expectedValue: ExpectsValue type, plurality: One, short, long, help }

    default_generator = \{} ->
        when default is
            Value default_value -> Ok defaultValue
            Generate generator -> Ok (generator {})
            NoDefault -> Err (MissingOption option)

    value_parser = \values ->
        value = try getMaybeValue values option

        when value is
            Err NoValue -> defaultGenerator {}
            Ok (Err NoValue) -> Err (NoValueProvidedForOption option)
            Ok (Ok val) ->
                parser val
                |> Result.mapErr \err -> InvalidOptionValue err option

    builderWithOptionParser option valueParser

## Add an optional option that takes a custom type to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err (InvalidValue Str)`
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
##     parseColor : Str -> Result Color [InvalidValue Str]
##     parseColor = \color ->
##         when color is
##             "green" -> Ok Green
##             "red" -> Ok Red
##             "blue" -> Ok Blue
##             other -> Err (InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Opt.maybe { short: "c", type: "color", parser: parseColor },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe : OptionConfigParams data -> CliBuilder (Result data [NoValue]) GetOptionsAction GetOptionsAction
maybe = \{ parser, type, short ? "", long ? "", help ? "" } ->
    option = { expectedValue: ExpectsValue type, plurality: Optional, short, long, help }

    value_parser = \values ->
        value = try getMaybeValue values option

        when value is
            Err NoValue -> Ok (Err NoValue)
            Ok (Err NoValue) -> Err (NoValueProvidedForOption option)
            Ok (Ok val) ->
                parser val
                |> Result.map Ok
                |> Result.mapErr \err -> InvalidOptionValue err option

    builderWithOptionParser option valueParser

## Add an option that takes a custom type and can be given multiple times
## to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err (InvalidValue Str)`
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
##     parseColor : Str -> Result Color [InvalidValue Str]
##     parseColor = \color ->
##         when color is
##             "green" -> Ok Green
##             "red" -> Ok Red
##             "blue" -> Ok Blue
##             other -> Err (InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Opt.list { short: "c", type: "color", parser: parseColor },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-c", "green", "--color=red"]
##     == SuccessfullyParsed [Green, Red]
## ```
list : OptionConfigParams data -> CliBuilder (List data) GetOptionsAction GetOptionsAction
list = \{ parser, type, short ? "", long ? "", help ? "" } ->
    option = { expectedValue: ExpectsValue type, plurality: Many, short, long, help }

    value_parser = \values ->
        List.mapTry values \value ->
            when value is
                Err NoValue -> Err (NoValueProvidedForOption option)
                Ok val ->
                    parser val
                    |> Result.mapErr \err -> InvalidOptionValue err option

    builderWithOptionParser option valueParser

## Add an optional flag to your CLI builder.
##
## Parsing arguments will fail if the flag is given more than once
## or if a value is provided to it, e.g. `--flag=value`.
##
## ```roc
## expect
##     { parser } =
##         Opt.flag { short: "f", long: "force" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-f"]
##     == SuccessfullyParsed Bool.true
## ```
flag : OptionConfigBaseParams -> CliBuilder Bool GetOptionsAction GetOptionsAction
flag = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedValue: NothingExpected, plurality: Optional, short, long, help }

    value_parser = \values ->
        value = try getMaybeValue values option

        when value is
            Err NoValue -> Ok Bool.false
            Ok (Err NoValue) -> Ok Bool.true
            Ok (Ok _val) -> Err (OptionDoesNotExpectValue option)

    builderWithOptionParser option valueParser

## Add a flag that can be given multiple times to your CLI builder.
##
## Parsing arguments will fail if this flag is ever given a value,
## e.g. `--flag=value`.
##
## ```roc
## expect
##     { parser } =
##         Opt.count { short: "f", long: "force" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-f", "--force", "-fff"]
##     == SuccessfullyParsed 5
## ```
count : OptionConfigBaseParams -> CliBuilder U64 GetOptionsAction GetOptionsAction
count = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedValue: NothingExpected, plurality: Many, short, long, help }

    value_parser = \values ->
        if values |> List.any Result.isOk then
            Err (OptionDoesNotExpectValue option)
        else
            Ok (List.len values)

    builderWithOptionParser option valueParser

## Add a required option that takes a string to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument
## or a value is not provided to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.str { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=abc"]
##     == SuccessfullyParsed "abc"
## ```
str : DefaultableOptionConfigBaseParams Str -> CliBuilder Str GetOptionsAction GetOptionsAction
str = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Ok, type: strTypeName, short, long, help, default }

## Add an optional option that takes a string to your CLI builder.
##
## Parsing arguments will fail if more than one instance of the argument
## is provided or there is no value given for the option call.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeStr { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_str : OptionConfigBaseParams -> CliBuilder (Result Str [NoValue]) GetOptionsAction GetOptionsAction
maybe_str = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Ok, type: strTypeName, short, long, help }

## Add an option that takes a string and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value.
##
## ```roc
## expect
##     { parser } =
##         Opt.strList { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "abc", "--answer", "def", "--answer=ghi"]
##     == SuccessfullyParsed ["abc", "def", "ghi"]
## ```
str_list : OptionConfigBaseParams -> CliBuilder (List Str) GetOptionsAction GetOptionsAction
str_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Ok, type: strTypeName, short, long, help }

## Add a required option that takes a `Dec` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.dec { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42.5"]
##     == SuccessfullyParsed 42.5
## ```
dec : DefaultableOptionConfigBaseParams Dec -> CliBuilder Dec GetOptionsAction GetOptionsAction
dec = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toDec, type: numTypeName, short, long, help, default }

## Add an optional option that takes a `Dec` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeDec { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_dec : OptionConfigBaseParams -> CliBuilder (Result Dec [NoValue]) GetOptionsAction GetOptionsAction
maybe_dec = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toDec, type: numTypeName, short, long, help }

## Add an option that takes a `Dec` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.decList { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "-3.0"]
##     == SuccessfullyParsed [1.0, 2.0, -3.0]
## ```
dec_list : OptionConfigBaseParams -> CliBuilder (List Dec) GetOptionsAction GetOptionsAction
dec_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toDec, type: numTypeName, short, long, help }

## Add a required option that takes a `F32` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.f32 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42.5"]
##     == SuccessfullyParsed 42.5
## ```
f32 : DefaultableOptionConfigBaseParams F32 -> CliBuilder F32 GetOptionsAction GetOptionsAction
f32 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toF32, type: numTypeName, short, long, help, default }

## Add an optional option that takes a `F32` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeF32 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_f32 : OptionConfigBaseParams -> CliBuilder (Result F32 [NoValue]) GetOptionsAction GetOptionsAction
maybe_f32 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toF32, type: numTypeName, short, long, help }

## Add an option that takes a `F32` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.f32List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "-3.0"]
##     == SuccessfullyParsed [1.0, 2.0, -3.0]
## ```
f32_list : OptionConfigBaseParams -> CliBuilder (List F32) GetOptionsAction GetOptionsAction
f32_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toF32, type: numTypeName, short, long, help }

## Add a required option that takes a `F64` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.f64 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42.5"]
##     == SuccessfullyParsed 42.5
## ```
f64 : DefaultableOptionConfigBaseParams F64 -> CliBuilder F64 GetOptionsAction GetOptionsAction
f64 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toF64, type: numTypeName, short, long, help, default }

## Add an optional option that takes a `F64` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeF64 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_f64 : OptionConfigBaseParams -> CliBuilder (Result F64 [NoValue]) GetOptionsAction GetOptionsAction
maybe_f64 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toF64, type: numTypeName, short, long, help }

## Add an option that takes a `F64` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.f64List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "-3.0"]
##     == SuccessfullyParsed [1.0, 2.0, -3.0]
## ```
f64_list : OptionConfigBaseParams -> CliBuilder (List F64) GetOptionsAction GetOptionsAction
f64_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toF64, type: numTypeName, short, long, help }

## Add a required option that takes a `U8` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u8 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
u8 : DefaultableOptionConfigBaseParams U8 -> CliBuilder U8 GetOptionsAction GetOptionsAction
u8 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toU8, type: numTypeName, short, long, help, default }

## Add an optional option that takes a `U8` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeU8 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_u8 : OptionConfigBaseParams -> CliBuilder (Result U8 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u8 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toU8, type: numTypeName, short, long, help }

## Add an option that takes a `U8` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u8List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
u8_list : OptionConfigBaseParams -> CliBuilder (List U8) GetOptionsAction GetOptionsAction
u8_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toU8, type: numTypeName, short, long, help }

## Add a required option that takes a `U16` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u16 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
u16 : DefaultableOptionConfigBaseParams U16 -> CliBuilder U16 GetOptionsAction GetOptionsAction
u16 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toU16, type: numTypeName, short, long, help, default }

## Add an optional option that takes a `U16` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeU16 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_u16 : OptionConfigBaseParams -> CliBuilder (Result U16 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u16 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toU16, type: numTypeName, short, long, help }

## Add an option that takes a `U16` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u16List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
u16_list : OptionConfigBaseParams -> CliBuilder (List U16) GetOptionsAction GetOptionsAction
u16_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toU16, type: numTypeName, short, long, help }

## Add a required option that takes a `U32` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u32 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
u32 : DefaultableOptionConfigBaseParams U32 -> CliBuilder U32 GetOptionsAction GetOptionsAction
u32 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toU32, type: numTypeName, short, long, help, default }

## Add an optional option that takes a `U32` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeU32 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_u32 : OptionConfigBaseParams -> CliBuilder (Result U32 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u32 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toU32, type: numTypeName, short, long, help }

## Add an option that takes a `U32` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u32List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
u32_list : OptionConfigBaseParams -> CliBuilder (List U32) GetOptionsAction GetOptionsAction
u32_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toU32, type: numTypeName, short, long, help }

## Add a required option that takes a `U64` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u64 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
u64 : DefaultableOptionConfigBaseParams U64 -> CliBuilder U64 GetOptionsAction GetOptionsAction
u64 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toU64, type: numTypeName, short, long, help, default }

## Add an optional option that takes a `U64` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeU64 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_u64 : OptionConfigBaseParams -> CliBuilder (Result U64 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u64 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toU64, type: numTypeName, short, long, help }

## Add an option that takes a `U64` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u64List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
u64_list : OptionConfigBaseParams -> CliBuilder (List U64) GetOptionsAction GetOptionsAction
u64_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toU64, type: numTypeName, short, long, help }

## Add a required option that takes a `U128` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.u128 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
u128 : DefaultableOptionConfigBaseParams U128 -> CliBuilder U128 GetOptionsAction GetOptionsAction
u128 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toU128, type: numTypeName, short, long, help, default }

## Add an optional option that takes a `U128` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeU128 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_u128 : OptionConfigBaseParams -> CliBuilder (Result U128 [NoValue]) GetOptionsAction GetOptionsAction
maybe_u128 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toU128, type: numTypeName, short, long, help }

## Add an option that takes a `U128` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.u128List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
u128_list : OptionConfigBaseParams -> CliBuilder (List U128) GetOptionsAction GetOptionsAction
u128_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toU128, type: numTypeName, short, long, help }

## Add a required option that takes an `I8` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i8 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
i8 : DefaultableOptionConfigBaseParams I8 -> CliBuilder I8 GetOptionsAction GetOptionsAction
i8 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toI8, type: numTypeName, short, long, help, default }

## Add an optional option that takes an `I8` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeI8 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_i8 : OptionConfigBaseParams -> CliBuilder (Result I8 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i8 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toI8, type: numTypeName, short, long, help }

## Add an option that takes an `I8` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i8List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
i8_list : OptionConfigBaseParams -> CliBuilder (List I8) GetOptionsAction GetOptionsAction
i8_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toI8, type: numTypeName, short, long, help }

## Add a required option that takes an `I16` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i16 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
i16 : DefaultableOptionConfigBaseParams I16 -> CliBuilder I16 GetOptionsAction GetOptionsAction
i16 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toI16, type: numTypeName, short, long, help, default }

## Add an optional option that takes an `I16` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeI16 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_i16 : OptionConfigBaseParams -> CliBuilder (Result I16 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i16 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toI16, type: numTypeName, short, long, help }

## Add an option that takes an `I16` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i16List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
i16_list : OptionConfigBaseParams -> CliBuilder (List I16) GetOptionsAction GetOptionsAction
i16_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toI16, type: numTypeName, short, long, help }

## Add a required option that takes an `I32` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i32 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
i32 : DefaultableOptionConfigBaseParams I32 -> CliBuilder I32 GetOptionsAction GetOptionsAction
i32 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toI32, type: numTypeName, short, long, help, default }

## Add an optional option that takes an `I32` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeI32 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_i32 : OptionConfigBaseParams -> CliBuilder (Result I32 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i32 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toI32, type: numTypeName, short, long, help }

## Add an option that takes an `I32` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i32List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
i32_list : OptionConfigBaseParams -> CliBuilder (List I32) GetOptionsAction GetOptionsAction
i32_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toI32, type: numTypeName, short, long, help }

## Add a required option that takes an `I64` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i64 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
i64 : DefaultableOptionConfigBaseParams I64 -> CliBuilder I64 GetOptionsAction GetOptionsAction
i64 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toI64, type: numTypeName, short, long, help, default }

## Add an optional option that takes an `I64` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeI64 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_i64 : OptionConfigBaseParams -> CliBuilder (Result I64 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i64 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toI64, type: numTypeName, short, long, help }

## Add an option that takes an `I64` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i64List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
i64_list : OptionConfigBaseParams -> CliBuilder (List I64) GetOptionsAction GetOptionsAction
i64_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toI64, type: numTypeName, short, long, help }

## Add a required option that takes an `I128` to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Opt.i128 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed 42
## ```
i128 : DefaultableOptionConfigBaseParams I128 -> CliBuilder I128 GetOptionsAction GetOptionsAction
i128 = \{ short ? "", long ? "", help ? "", default ? NoDefault } -> single { parser: Str.toI128, type: numTypeName, short, long, help, default }

## Add an optional option that takes an `I128` to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Opt.maybeI128 { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed (Err NoValue)
## ```
maybe_i128 : OptionConfigBaseParams -> CliBuilder (Result I128 [NoValue]) GetOptionsAction GetOptionsAction
maybe_i128 = \{ short ? "", long ? "", help ? "" } -> maybe { parser: Str.toI128, type: numTypeName, short, long, help }

## Add an option that takes an `I128` and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Opt.i128List { long: "answer" },
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer=2", "--answer", "3"]
##     == SuccessfullyParsed [1, 2, 3]
## ```
i128_list : OptionConfigBaseParams -> CliBuilder (List I128) GetOptionsAction GetOptionsAction
i128_list = \{ short ? "", long ? "", help ? "" } -> list { parser: Str.toI128, type: numTypeName, short, long, help }
