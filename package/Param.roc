interface Param
    exposes [
        str,
        maybeStr,
        strList,
        num,
        maybeNum,
        numList,
        custom,
        maybeCustom,
        customList,
    ]
    imports [
        Builder.{
            GetParamsAction,
            StopCollectingAction,
            CliBuilder,
            GetParamsAction,
            StopCollectingAction,
        },
        Base.{
            ArgExtractErr,
            ParameterConfigParams,
            ParameterConfig,
        },
        Parser.{ ArgValue },
        Extract.{ extractParamValues },
    ]

updateBuilderWithParameterParser : CliBuilder (a -> state) action, ParameterConfig, (List Str -> Result a ArgExtractErr) -> CliBuilder state nextAction
updateBuilderWithParameterParser = \builder, param, valueParser ->
    builder
    |> Builder.addParameters [param]
    |> Builder.updateParser \{ data, remainingArgs } ->
        { values, remainingArgs: restOfArgs } <- extractParamValues { args: remainingArgs, param }
            |> Result.try
        value <- valueParser values
            |> Result.try

        Ok { data: data value, remainingArgs: restOfArgs }

singleParam : ParameterConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (a -> state) {}action -> CliBuilder state GetParamsAction)
singleParam = \param, valueParser ->
    \builder ->
        values <- updateBuilderWithParameterParser builder param
        when List.first values is
            Ok single -> valueParser single
            Err ListWasEmpty -> Err (MissingParam param)

maybeParam : ParameterConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (Result a [NoValue] -> state) {}action -> CliBuilder state GetParamsAction)
maybeParam = \param, valueParser ->
    \builder ->
        values <- updateBuilderWithParameterParser builder param
        when List.first values is
            Ok single -> valueParser single |> Result.map Ok
            Err ListWasEmpty -> Ok (Err NoValue)

listParam : ParameterConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (List a -> state) {}action -> CliBuilder state StopCollectingAction)
listParam = \option, valueParser ->
    \builder ->
        values <- updateBuilderWithParameterParser builder option
        values |> List.mapTry valueParser

parseNumParam : ParameterConfig -> (Str -> Result I64 ArgExtractErr)
parseNumParam = \param ->
    \value -> Str.toI64 value |> Result.mapErr \_err -> InvalidNumParam param

parseCustomParam : ParameterConfig, (Str -> Result a [InvalidValue Str]) -> (Str -> Result a ArgExtractErr)
parseCustomParam = \param, valueParser ->
    \value -> valueParser value |> Result.mapErr \InvalidValue reason -> InvalidCustomParam param reason

## Add a required string parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided.
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
##     { parser } =
##         Cli.weave {
##             answer: <- Param.str { name: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "abc"]
##     == SuccessfullyParsed { answer: "abc" }
## ```
str : ParameterConfigParams -> (CliBuilder (Str -> state) {}action -> CliBuilder state GetParamsAction)
str = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: One }
    singleParam param Ok

## Add an optional string parameter to your CLI builder.
##
## Parsing arguments cannot fail because of this parameter.
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
##     { parser } =
##         Cli.weave {
##             answer: <- Param.maybeStr { name: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed { answer: Err NoValue }
## ```
maybeStr : ParameterConfigParams -> (CliBuilder (ArgValue -> state) {}action -> CliBuilder state GetParamsAction)
maybeStr = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Optional }
    maybeParam param Ok

## Add a string parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments cannot fail because of this parameter.
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
##     { parser } =
##         Cli.weave {
##             answer: <- Param.strList { name: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "abc", "def", "ghi"]
##     == SuccessfullyParsed { answer: ["abc", "def", "ghi"] }
## ```
strList : ParameterConfigParams -> (CliBuilder (List Str -> state) {}action -> CliBuilder state StopCollectingAction)
strList = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Many }
    listParam param Ok

## Add a required number parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not provided
## or it is not a valid number.
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
##     { parser } =
##         Cli.weave {
##             answer: <- Param.num { name: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "42"]
##     == SuccessfullyParsed { answer: 42 }
## ```
num : ParameterConfigParams -> (CliBuilder (I64 -> state) {}action -> CliBuilder state GetParamsAction)
num = \{ name, help ? "" } ->
    param = { name, help, type: Num, plurality: One }
    singleParam param (parseNumParam param)

## Add an optional number parameter to your CLI builder.
##
## Parsing arguments will fail if the parameter is not a valid number.
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
##     { parser } =
##         Cli.weave {
##             answer: <- Param.maybeNum { name: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed { answer: Err NoValue }
## ```
maybeNum : ParameterConfigParams -> (CliBuilder (Result I64 [NoValue] -> state) {}action -> CliBuilder state GetParamsAction)
maybeNum = \{ name, help ? "" } ->
    param = { name, help, type: Num, plurality: Optional }
    maybeParam param (parseNumParam param)

## Add a number parameter that can be provided multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any of the parameters are
## not valid numbers.
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
##     { parser } =
##         Cli.weave {
##             answer: <- Param.maybeNum { name: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "123", "456", "789"]
##     == SuccessfullyParsed { answer: [123, 456, 789] }
## ```
numList : ParameterConfigParams -> (CliBuilder (List I64 -> state) {}action -> CliBuilder state StopCollectingAction)
numList = \{ name, help ? "" } ->
    param = { name, help, type: Num, plurality: Many }
    listParam param (parseNumParam param)

## Add a required parameter of a custom type to your CLI builder.
##
## You need to provide a type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err (InvalidValue Str)`
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
##     parseColor : Str -> Result Color [InvalidValue Str]
##     parseColor = \color ->
##         when color is
##             "green" -> Ok Green
##             "red" -> Ok Red
##             "blue" -> Ok Blue
##             other -> Err (InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Cli.weave {
##             answer: <- Param.custom { name: "answer", typeName: "Color", parser: parseColor },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "blue"]
##     == SuccessfullyParsed { answer: Blue }
## ```
custom : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }ParameterConfigParams -> (CliBuilder (a -> state) {}action -> CliBuilder state GetParamsAction)
custom = \{ name, help ? "", typeName, parser } ->
    param = { name, help, type: Custom typeName, plurality: One }
    singleParam param (parseCustomParam param parser)

## Add an optional parameter of a custom type to your CLI builder.
##
## You need to provide a type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err (InvalidValue Str)`
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
##     parseColor : Str -> Result Color [InvalidValue Str]
##     parseColor = \color ->
##         when color is
##             "green" -> Ok Green
##             "red" -> Ok Red
##             "blue" -> Ok Blue
##             other -> Err (InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Cli.weave {
##             answer: <- Param.maybeCustom { name: "answer", typeName: "Color", parser: parseColor },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed { answer: Err NoValue }
## ```
maybeCustom : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }ParameterConfigParams -> (CliBuilder (Result a [NoValue] -> state) {}action -> CliBuilder state GetParamsAction)
maybeCustom = \{ name, help ? "", typeName, parser } ->
    param = { name, help, type: Custom typeName, plurality: Optional }
    maybeParam param (parseCustomParam param parser)

## Add a parameter of a custom type that can be provided
## multiple times to your CLI builder.
##
## You need to provide a type name for your help messages as well as a
## parser for said type. The parser needs to return an `Err (InvalidValue Str)`
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
##     parseColor : Str -> Result Color [InvalidValue Str]
##     parseColor = \color ->
##         when color is
##             "green" -> Ok Green
##             "red" -> Ok Red
##             "blue" -> Ok Blue
##             other -> Err (InvalidValue "'$(other)' is not a valid color, must be green, red, or blue")
##
##     { parser } =
##         Cli.weave {
##             answer: <- Param.customList { name: "answer", typeName: "Color", parser: parseColor },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "blue", "red", "green"]
##     == SuccessfullyParsed { answer: [Blue, Red, Green] }
## ```
customList : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }ParameterConfigParams -> (CliBuilder (List a -> state) {}action -> CliBuilder state StopCollectingAction)
customList = \{ name, help ? "", typeName, parser } ->
    param = { name, help, type: Custom typeName, plurality: Many }
    listParam param (parseCustomParam param parser)
