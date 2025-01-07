module [
    single,
    maybe,
    list,
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
import Builder exposing [
    CliBuilder,
    GetParamsAction,
    StopCollectingAction,
]
import Base exposing [
    ArgExtractErr,
    ParameterConfigBaseParams,
    DefaultableParameterConfigBaseParams,
    ParameterConfigParams,
    DefaultableParameterConfigParams,
    ParameterConfig,
    str_type_name,
    num_type_name,
]
import Parser exposing [ArgValue]
import Extract exposing [extract_param_values]

builder_with_parameter_parser : ParameterConfig, (List Arg -> Result data ArgExtractErr) -> CliBuilder data from_action to_action
builder_with_parameter_parser = \param, value_parser ->
    arg_parser = \args ->
        { values, remaining_args } = try(extract_param_values, { args, param })
        data = try(value_parser, values)

        Ok({ data, remaining_args })

    Builder.from_arg_parser(arg_parser)
    |> Builder.add_parameter(param)

## Add a required parameter of a custom type to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err(InvalidValue(Str))`
## on failure, where the `Str` is the reason the parsing failed that will
## get displayed in the incorrect usage message.
##
## Parsing arguments will fail if the parameter fails to parse or
## is not provided.
##
## Parameters must be provided last after any option or subcommand fields,
## as they are parsed last of the three extracted values, and parameter
## list fields cannot be followed by any other fields. This is enforced
## using the type state pattern, where we encode the state of the program
## into its types. If you're curious, check the internal `Builder`
## module to see how this works using the `action` type variable.
##
## ```roc
## expect
##     Color : [Green, Red, Blue]
##
##     parse_color : Arg -> Result Color [InvalidValue Str, InvalidUtf8]
##     parse_color = \color ->
##         when Arg.to_str(color) is
##             Ok("green") -> Ok(Green)
##             Ok("red") -> Ok(Red)
##             Ok("blue") -> Ok(Blue)
##             other -> Err(InvalidValue("'$(other)' is not a valid color, must be green, red, or blue"))
##
##     { parser } =
##         Param.single({ name: "answer", type: "color", parser: parse_color })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "blue"])
##     == SuccessfullyParsed(Blue)
## ```
single : DefaultableParameterConfigParams data -> CliBuilder data {}action GetParamsAction
single = \{ parser, type, name, help ?? "", default ?? NoDefault } ->
    param = { name, type, help, plurality: One }

    default_generator = \{} ->
        when default is
            Value(default_value) -> Ok(default_value)
            Generate(generator) -> Ok(generator({}))
            NoDefault -> Err(MissingParam(param))

    value_parser = \values ->
        when List.first(values) is
            Err(ListWasEmpty) -> default_generator({})
            Ok(single_value) ->
                parser(single_value)
                |> Result.map_err(\err -> InvalidParamValue(err, param))

    builder_with_parameter_parser(param, value_parser)

## Add an optional parameter of a custom type to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err(InvalidValue(Str))`
## on failure, where the `Str` is the reason the parsing failed that will
## get displayed in the incorrect usage message.
##
## Parsing arguments will fail if the parameter fails to parse.
##
## Parameters must be provided last after any option or subcommand fields,
## as they are parsed last of the three extracted values, and parameter
## list fields cannot be followed by any other fields. This is enforced
## using the type state pattern, where we encode the state of the program
## into its types. If you're curious, check the internal `Builder`
## module to see how this works using the `action` type variable.
##
## ```roc
## expect
##     Color : [Green, Red, Blue]
##
##     parse_color : Arg -> Result Color [InvalidValue Str, InvalidUtf8]
##     parse_color = \color ->
##         when Arg.to_str(color) is
##             Ok("green") -> Ok(Green)
##             Ok("red") -> Ok(Red)
##             Ok("blue") -> Ok(Blue)
##             other -> Err(InvalidValue("'$(other)' is not a valid color, must be green, red, or blue"))
##
##     { parser } =
##         Param.maybe({ name: "answer", type: "color", parser: parse_color })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe : ParameterConfigParams data -> CliBuilder (Result data [NoValue]) {}action GetParamsAction
maybe = \{ parser, type, name, help ?? "" } ->
    param = { name, type, help, plurality: Optional }

    value_parser = \values ->
        when List.first(values) is
            Err(ListWasEmpty) -> Ok(Err(NoValue))
            Ok(single_value) ->
                parser(single_value)
                |> Result.map(Ok)
                |> Result.map_err(\err -> InvalidParamValue(err, param))

    builder_with_parameter_parser(param, value_parser)

## Add a parameter of a custom type that can be provided
## multiple times to your CLI builder.
##
## You need to provide a kebab-case type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err(InvalidValue(Str))`
## on failure, where the `Str` is the reason the parsing failed that will
## get displayed in the incorrect usage message.
##
## Parsing arguments will fail if any of the values fail to parse.
##
## Parameters must be provided last after any option or subcommand fields,
## as they are parsed last of the three extracted values, and parameter
## list fields cannot be followed by any other fields. This is enforced
## using the type state pattern, where we encode the state of the program
## into its types. If you're curious, check the internal `Builder`
## module to see how this works using the `action` type variable.
##
## ```roc
## expect
##     Color : [Green, Red, Blue]
##
##     parse_color : Arg -> Result Color [InvalidValue Str, InvalidUtf8]
##     parse_color = \color ->
##         when Arg.to_str(color) is
##             Ok("green") -> Ok(Green)
##             Ok("red") -> Ok(Red)
##             Ok("blue") -> Ok(Blue)
##             other -> Err(InvalidValue("'$(other)' is not a valid color, must be green, red, or blue"))
##
##     { parser } =
##         Param.list({ name: "answer", type: "color", parser: parse_color })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "blue", "red", "green"])
##     == SuccessfullyParsed([Blue, Red, Green])
## ```
list : ParameterConfigParams data -> CliBuilder (List data) {}action StopCollectingAction
list = \{ parser, type, name, help ?? "" } ->
    param = { name, type, help, plurality: Many }

    value_parser = \values ->
        List.map_try(values, parser)
        |> Result.map_err(\err -> InvalidParamValue(err, param))

    builder_with_parameter_parser(param, value_parser)

## Add a required [Arg] parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided.
##
## ```roc
## expect
##     { parser } =
##         Param.arg({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "abc"])
##     == SuccessfullyParsed(Arg.from_str("abc"))
## ```
arg : DefaultableParameterConfigBaseParams Arg -> CliBuilder Arg {}action GetParamsAction
arg = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: Ok,
        type: str_type_name,
        name,
        help,
        default,
    })

## Add an optional [Arg] parameter to your CLI builder.
##
## Parsing arguments cannot fail because of this parameter.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_arg({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_arg : ParameterConfigBaseParams -> CliBuilder ArgValue {}action GetParamsAction
maybe_arg = \{ name, help ?? "" } ->
    maybe({
        parser: Ok,
        type: str_type_name,
        name,
        help,
    })

## Add an [Arg] parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments cannot fail because of this parameter.
##
## ```roc
## expect
##     { parser } =
##         Param.arg_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "abc", "def", "ghi"])
##     == SuccessfullyParsed(List.map(["abc", "def", "ghi"], Arg.from_str))
## ```
arg_list : ParameterConfigBaseParams -> CliBuilder (List Arg) {}action StopCollectingAction
arg_list = \{ name, help ?? "" } ->
    list({
        parser: Ok,
        type: str_type_name,
        name,
        help,
    })

## Add a required byte list parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided.
##
## ```roc
## expect
##     { parser } =
##         Param.arg({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "abc"])
##     == SuccessfullyParsed([97, 98, 99])
## ```
bytes : DefaultableParameterConfigBaseParams (List U8) -> CliBuilder (List U8) {}action GetParamsAction
bytes = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Ok(Arg.to_bytes(a)),
        type: str_type_name,
        name,
        help,
        default,
    })

## Add an optional byte list parameter to your CLI builder.
##
## Parsing arguments cannot fail because of this parameter.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_bytes({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_bytes : ParameterConfigBaseParams -> CliBuilder (Result (List U8) [NoValue]) {}action GetParamsAction
maybe_bytes = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Ok(Arg.to_bytes(a)),
        type: str_type_name,
        name,
        help,
    })

## Add a byte list parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments cannot fail because of this parameter.
##
## ```roc
## expect
##     { parser } =
##         Param.bytes_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "abc", "def", "ghi"])
##     == SuccessfullyParsed([[97, 98, 99], [100, 101, 102], [103, 104, 105]])
## ```
bytes_list : ParameterConfigBaseParams -> CliBuilder (List (List U8)) {}action StopCollectingAction
bytes_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Ok(Arg.to_bytes(a)),
        type: str_type_name,
        name,
        help,
    })

## Add a required string parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or if it is not valid UTF-8.
##
## ```roc
## expect
##     { parser } =
##         Param.str({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "abc"])
##     == SuccessfullyParsed("abc")
## ```
str : DefaultableParameterConfigBaseParams Str -> CliBuilder Str {}action GetParamsAction
str = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: Arg.to_str,
        type: str_type_name,
        name,
        help,
        default,
    })

## Add an optional string parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not valid UTF-8.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_str({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_str : ParameterConfigBaseParams -> CliBuilder (Result Str [NoValue]) {}action GetParamsAction
maybe_str = \{ name, help ?? "" } ->
    maybe({
        parser: Arg.to_str,
        type: str_type_name,
        name,
        help,
    })

## Add a string parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the arguments are not valid UTF-8.
##
## ```roc
## expect
##     { parser } =
##         Param.str_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "abc", "def", "ghi"])
##     == SuccessfullyParsed(["abc", "def", "ghi"])
## ```
str_list : ParameterConfigBaseParams -> CliBuilder (List Str) {}action StopCollectingAction
str_list = \{ name, help ?? "" } ->
    list({
        parser: Arg.to_str,
        type: str_type_name,
        name,
        help,
    })

## Add a required `Dec` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.dec({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42.5"])
##     == SuccessfullyParsed(42.5)
## ```
dec : DefaultableParameterConfigBaseParams Dec -> CliBuilder Dec {}action GetParamsAction
dec = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_dec),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `Dec` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_dec({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_dec : ParameterConfigBaseParams -> CliBuilder (Result Dec [NoValue]) {}action GetParamsAction
maybe_dec = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_dec),
        type: num_type_name,
        name,
        help,
    })

## Add a `Dec` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.dec_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "--", "-56.0"])
##     == SuccessfullyParsed([12.0, 34.0, -56.0])
## ```
dec_list : ParameterConfigBaseParams -> CliBuilder (List Dec) {}action StopCollectingAction
dec_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_dec),
        type: num_type_name,
        name,
        help,
    })

## Add a required `F32` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.f32({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42.5"])
##     == SuccessfullyParsed(42.5)
## ```
f32 : DefaultableParameterConfigBaseParams F32 -> CliBuilder F32 {}action GetParamsAction
f32 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_f32),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `F32` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_f32({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_f32 : ParameterConfigBaseParams -> CliBuilder (Result F32 [NoValue]) {}action GetParamsAction
maybe_f32 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_f32),
        type: num_type_name,
        name,
        help,
    })

## Add a `F32` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.f32_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "--", "-56.0"])
##     == SuccessfullyParsed([12.0, 34.0, -56.0])
## ```
f32_list : ParameterConfigBaseParams -> CliBuilder (List F32) {}action StopCollectingAction
f32_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_f32),
        type: num_type_name,
        name,
        help,
    })

## Add a required `F64` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.f64({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42.5"])
##     == SuccessfullyParsed(42.5)
## ```
f64 : DefaultableParameterConfigBaseParams F64 -> CliBuilder F64 {}action GetParamsAction
f64 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_f64),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `F64` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_f64({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_f64 : ParameterConfigBaseParams -> CliBuilder (Result F64 [NoValue]) {}action GetParamsAction
maybe_f64 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_f64),
        type: num_type_name,
        name,
        help,
    })

## Add a `F64` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.f64_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "--", "-56.0"])
##     == SuccessfullyParsed([12, 34, -56.0])
## ```
f64_list : ParameterConfigBaseParams -> CliBuilder (List F64) {}action StopCollectingAction
f64_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_f64),
        type: num_type_name,
        name,
        help,
    })

## Add a required `U8` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.u8({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
u8 : DefaultableParameterConfigBaseParams U8 -> CliBuilder U8 {}action GetParamsAction
u8 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u8),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `U8` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_u8({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u8 : ParameterConfigBaseParams -> CliBuilder (Result U8 [NoValue]) {}action GetParamsAction
maybe_u8 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u8),
        type: num_type_name,
        name,
        help,
    })

## Add a `U8` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.u8_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "56"])
##     == SuccessfullyParsed([12, 34, 56])
## ```
u8_list : ParameterConfigBaseParams -> CliBuilder (List U8) {}action StopCollectingAction
u8_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u8),
        type: num_type_name,
        name,
        help,
    })

## Add a required `U16` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.u16({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
u16 : DefaultableParameterConfigBaseParams U16 -> CliBuilder U16 {}action GetParamsAction
u16 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u16),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `U16` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_u16({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u16 : ParameterConfigBaseParams -> CliBuilder (Result U16 [NoValue]) {}action GetParamsAction
maybe_u16 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u16),
        type: num_type_name,
        name,
        help,
    })

## Add a `U16` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.u16_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "56"])
##     == SuccessfullyParsed([12, 34, 56])
## ```
u16_list : ParameterConfigBaseParams -> CliBuilder (List U16) {}action StopCollectingAction
u16_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u16),
        type: num_type_name,
        name,
        help,
    })

## Add a required `U32` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.u32({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
u32 : DefaultableParameterConfigBaseParams U32 -> CliBuilder U32 {}action GetParamsAction
u32 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u32),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `U32` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_u32({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u32 : ParameterConfigBaseParams -> CliBuilder (Result U32 [NoValue]) {}action GetParamsAction
maybe_u32 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u32),
        type: num_type_name,
        name,
        help,
    })

## Add a `U32` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.u32_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "56"])
##     == SuccessfullyParsed([12, 34, 56])
## ```
u32_list : ParameterConfigBaseParams -> CliBuilder (List U32) {}action StopCollectingAction
u32_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u32),
        type: num_type_name,
        name,
        help,
    })

## Add a required `U64` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.u64({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
u64 : DefaultableParameterConfigBaseParams U64 -> CliBuilder U64 {}action GetParamsAction
u64 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u64),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `U64` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_u64({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u64 : ParameterConfigBaseParams -> CliBuilder (Result U64 [NoValue]) {}action GetParamsAction
maybe_u64 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u64),
        type: num_type_name,
        name,
        help,
    })

## Add a `U64` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.u64_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "56"])
##     == SuccessfullyParsed([12, 34, 56])
## ```
u64_list : ParameterConfigBaseParams -> CliBuilder (List U64) {}action StopCollectingAction
u64_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u64),
        type: num_type_name,
        name,
        help,
    })

## Add a required `U128` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.u128({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
u128 : DefaultableParameterConfigBaseParams U128 -> CliBuilder U128 {}action GetParamsAction
u128 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u128),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `U128` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_u128({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_u128 : ParameterConfigBaseParams -> CliBuilder (Result U128 [NoValue]) {}action GetParamsAction
maybe_u128 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u128),
        type: num_type_name,
        name,
        help,
    })

## Add a `U128` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.u128_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "56"])
##     == SuccessfullyParsed([12, 34, 56])
## ```
u128_list : ParameterConfigBaseParams -> CliBuilder (List U128) {}action StopCollectingAction
u128_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_u128),
        type: num_type_name,
        name,
        help,
    })

## Add a required `I8` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.i8({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
i8 : DefaultableParameterConfigBaseParams I8 -> CliBuilder I8 {}action GetParamsAction
i8 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i8),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `I8` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_i8({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i8 : ParameterConfigBaseParams -> CliBuilder (Result I8 [NoValue]) {}action GetParamsAction
maybe_i8 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i8),
        type: num_type_name,
        name,
        help,
    })

## Add an `I8` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.i8_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "--", "-56"])
##     == SuccessfullyParsed([12, 34, -56])
## ```
i8_list : ParameterConfigBaseParams -> CliBuilder (List I8) {}action StopCollectingAction
i8_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i8),
        type: num_type_name,
        name,
        help,
    })

## Add a required `I16` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.i16({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
i16 : DefaultableParameterConfigBaseParams I16 -> CliBuilder I16 {}action GetParamsAction
i16 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i16),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `I16` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_i16({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i16 : ParameterConfigBaseParams -> CliBuilder (Result I16 [NoValue]) {}action GetParamsAction
maybe_i16 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i16),
        type: num_type_name,
        name,
        help,
    })

## Add an `I16` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.i16_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "--", "-56"])
##     == SuccessfullyParsed([12, 34, -56])
## ```
i16_list : ParameterConfigBaseParams -> CliBuilder (List I16) {}action StopCollectingAction
i16_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i16),
        type: num_type_name,
        name,
        help,
    })

## Add a required `I32` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.i32({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
i32 : DefaultableParameterConfigBaseParams I32 -> CliBuilder I32 {}action GetParamsAction
i32 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i32),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `I32` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_i32({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i32 : ParameterConfigBaseParams -> CliBuilder (Result I32 [NoValue]) {}action GetParamsAction
maybe_i32 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i32),
        type: num_type_name,
        name,
        help,
    })

## Add an `I32` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.i32_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "--", "-56"])
##     == SuccessfullyParsed([12, 34, -56])
## ```
i32_list : ParameterConfigBaseParams -> CliBuilder (List I32) {}action StopCollectingAction
i32_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i32),
        type: num_type_name,
        name,
        help,
    })

## Add a required `I64` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.i64({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
i64 : DefaultableParameterConfigBaseParams I64 -> CliBuilder I64 {}action GetParamsAction
i64 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i64),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `I64` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_i64({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i64 : ParameterConfigBaseParams -> CliBuilder (Result I64 [NoValue]) {}action GetParamsAction
maybe_i64 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i64),
        type: num_type_name,
        name,
        help,
    })

## Add an `I64` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.i64_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "--", "-56"])
##     == SuccessfullyParsed([12, 34, -56])
## ```
i64_list : ParameterConfigBaseParams -> CliBuilder (List I64) {}action StopCollectingAction
i64_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i64),
        type: num_type_name,
        name,
        help,
    })

## Add a required `I128` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.i128({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "42"])
##     == SuccessfullyParsed(42)
## ```
i128 : DefaultableParameterConfigBaseParams I128 -> CliBuilder I128 {}action GetParamsAction
i128 = \{ name, help ?? "", default ?? NoDefault } ->
    single({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i128),
        type: num_type_name,
        name,
        help,
        default,
    })

## Add an optional `I128` parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
##
## ```roc
## expect
##     { parser } =
##         Param.maybe_i128({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example"])
##     == SuccessfullyParsed(Err(NoValue))
## ```
maybe_i128 : ParameterConfigBaseParams -> CliBuilder (Result I128 [NoValue]) {}action GetParamsAction
maybe_i128 = \{ name, help ?? "" } ->
    maybe({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i128),
        type: num_type_name,
        name,
        help,
    })

## Add an `I128` parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
##
## ```roc
## expect
##     { parser } =
##         Param.i128_list({ name: "answer" })
##         |> Cli.finish({ name: "example" })
##         |> Cli.assert_valid
##
##     parser(["example", "12", "34", "--", "-56"])
##     == SuccessfullyParsed([12, 34, -56])
## ```
i128_list : ParameterConfigBaseParams -> CliBuilder (List I128) {}action StopCollectingAction
i128_list = \{ name, help ?? "" } ->
    list({
        parser: \a -> Arg.to_str(a) |> Result.try(Str.to_i128),
        type: num_type_name,
        name,
        help,
    })
