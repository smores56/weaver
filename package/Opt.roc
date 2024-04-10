## Options that your CLI will parse as fields in your config.
interface Opt
    exposes [
        num,
        str,
        custom,
        maybeNum,
        maybeStr,
        maybeCustom,
        numList,
        strList,
        customList,
        flag,
        count,
    ]
    imports [
        Builder.{ CliBuilder, GetOptionsAction },
        Base.{ ArgExtractErr, OptionConfigParams, OptionConfig },
        Extract.{ extractOptionValues },
        Parser.{ ArgValue },
    ]

updateBuilderWithOptionParser : CliBuilder (a -> state) action, OptionConfig, (List ArgValue -> Result a ArgExtractErr) -> CliBuilder state nextAction
updateBuilderWithOptionParser = \builder, option, valueParser ->
    builder
    |> Builder.addOptions [option]
    |> Builder.updateParser \{ data, remainingArgs } ->
        { values, remainingArgs: restOfArgs } <- extractOptionValues { args: remainingArgs, option }
            |> Result.try
        value <- valueParser values
            |> Result.try

        Ok { data: data value, remainingArgs: restOfArgs }

getSingleValue : List ArgValue, OptionConfig -> Result ArgValue ArgExtractErr
getSingleValue = \values, option ->
    when values is
        [] -> Err (MissingOption option)
        [single] -> Ok single
        [..] -> Err (OptionCanOnlyBeSetOnce option)

getMaybeValue : List ArgValue, OptionConfig -> Result (Result ArgValue [NoValue]) ArgExtractErr
getMaybeValue = \values, option ->
    when values is
        [] -> Ok (Err NoValue)
        [single] -> Ok (Ok single)
        [..] -> Err (OptionCanOnlyBeSetOnce option)

singleOption : OptionConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
singleOption = \option, valueParser ->
    \builder ->
        values <- updateBuilderWithOptionParser builder option
        value <- getSingleValue values option
            |> Result.try

        when value is
            Ok val -> valueParser val
            Err NoValue -> Err (NoValueProvidedForOption option)

maybeOption : OptionConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (Result a [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeOption = \option, valueParser ->
    \builder ->
        values <- updateBuilderWithOptionParser builder option
        value <- getMaybeValue values option
            |> Result.try

        when value is
            Err NoValue -> Ok (Err NoValue)
            Ok (Err NoValue) -> Err (NoValueProvidedForOption option)
            Ok (Ok val) -> valueParser val |> Result.map Ok

listOption : OptionConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (List a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
listOption = \option, valueParser ->
    \builder ->
        values <- updateBuilderWithOptionParser builder option
        values
        |> List.mapTry \value ->
            when value is
                Ok val -> valueParser val
                Err NoValue -> Err (NoValueProvidedForOption option)

parseNumArgValue : OptionConfig -> (Str -> Result I64 ArgExtractErr)
parseNumArgValue = \option ->
    \value ->
        Str.toI64 value |> Result.mapErr \_ -> InvalidNumArg option

parseCustomArgValue : OptionConfig, (Str -> Result a [InvalidValue Str]) -> (Str -> Result a ArgExtractErr)
parseCustomArgValue = \option, parser ->
    \value ->
        parser value |> Result.mapErr \InvalidValue reason -> InvalidCustomArg option reason

## Add a required option that takes a number to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument,
## a value is not provided to the option, or the value is not a number.
##
## ```roc
## expect
##     { parser } =
##         Cli.weave {
##             answer: <- Opt.num { long: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=42"]
##     == SuccessfullyParsed { answer: 42 }
## ```
num : OptionConfigParams -> (CliBuilder (I64 -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
num = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Num, plurality: One, short, long, help }
    singleOption option (parseNumArgValue option)

## Add an optional option that takes a number to your CLI builder.
##
## Parsing arguments will fail if a value is not provided to the option,
## the value is not a number, or there is more than one call to the option.
##
## ```roc
## expect
##     { parser } =
##         Cli.weave {
##             answer: <- Opt.maybeNum { long: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed { answer: Err NoValue }
## ```
maybeNum : OptionConfigParams -> (CliBuilder (Result I64 [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeNum = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Num, plurality: Optional, short, long, help }
    maybeOption option (parseNumArgValue option)

## Add an option that takes a number and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value, or the values are not all numbers.
##
## ```roc
## expect
##     { parser } =
##         Cli.weave {
##             answer: <- Opt.numList { long: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "1", "--answer", "2", "--answer=3"]
##     == SuccessfullyParsed { answer: [1, 2, 3] }
## ```
numList : OptionConfigParams -> (CliBuilder (List I64 -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
numList = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Num, plurality: Many, short, long, help }
    listOption option (parseNumArgValue option)

## Add a required option that takes a string to your CLI builder.
##
## Parsing arguments will fail if the option is not given as an argument
## or a value is not provided to the option.
##
## ```roc
## expect
##     { parser } =
##         Cli.weave {
##             answer: <- Opt.str { long: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "--answer=abc"]
##     == SuccessfullyParsed { answer: "abc" }
## ```
str : OptionConfigParams -> (CliBuilder (Str -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
str = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Str, plurality: One, short, long, help }
    singleOption option Ok

## Add an optional option that takes a string to your CLI builder.
##
## Parsing arguments will fail if more than one instance of the argument
## is provided or there is no value given for the option call.
##
## ```roc
## expect
##     { parser } =
##         Cli.weave {
##             answer: <- Opt.maybeStr { long: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed { answer: Err NoValue }
## ```
maybeStr : OptionConfigParams -> (CliBuilder (Result Str [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeStr = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Str, plurality: Optional, short, long, help }
    maybeOption option Ok

## Add an option that takes a string and can be given multiple times
## to your CLI builder.
##
## Parsing arguments will fail if any calls of the option don't provide
## a value.
##
## ```roc
## expect
##     { parser } =
##         Cli.weave {
##             answer: <- Opt.strList { long: "answer" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-a", "abc", "--answer", "def", "--answer=ghi"]
##     == SuccessfullyParsed { answer: ["abc", "def", "ghi"] }
## ```
strList : OptionConfigParams -> (CliBuilder (List Str -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
strList = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Str, plurality: Many, short, long, help }
    listOption option Ok

## Add a required option that takes a custom type to your CLI builder.
##
## You need to provide a type name for your help messages as well as a
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
##         Cli.weave {
##             color: <- Opt.custom { short: "c", typeName: "Color", parser: parseColor },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-c", "green"]
##     == SuccessfullyParsed { answer: Green }
## ```
custom : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
custom = \{ short ? "", long ? "", help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: One, short, long, help }
    singleOption option (parseCustomArgValue option parser)

## Add an optional option that takes a custom type to your CLI builder.
##
## You need to provide a type name for your help messages as well as a
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
##         Cli.weave {
##             color: <- Opt.maybeCustom { short: "c", typeName: "Color", parser: parseColor },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example"]
##     == SuccessfullyParsed { answer: Err NoValue }
## ```
maybeCustom : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (Result a [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeCustom = \{ short ? "", long ? "", help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: Optional, short, long, help }
    maybeOption option (parseCustomArgValue option parser)

## Add an option that takes a custom type and can be given multiple times
## to your CLI builder.
##
## You need to provide a type name for your help messages as well as a
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
##         Cli.weave {
##             color: <- Opt.customList { short: "c", typeName: "Color", parser: parseColor },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-c", "green", "--color=red"]
##     == SuccessfullyParsed { answer: [Green, Red] }
## ```
customList : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (List a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
customList = \{ short ? "", long ? "", help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: Many, short, long, help }
    listOption option (parseCustomArgValue option parser)

## Add an optional flag to your CLI builder.
##
## Parsing arguments will fail if the flag is given more than once
## or if a value is provided to it, e.g. `--flag=value`.
##
## ```roc
## expect
##     { parser } =
##         Cli.weave {
##             force: <- Opt.flag { short: "f", long: "force" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-f"]
##     == SuccessfullyParsed { force: Bool.true }
## ```
flag : OptionConfigParams -> (CliBuilder (Bool -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
flag = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: None, plurality: Optional, short, long, help }

    \builder ->
        values <- updateBuilderWithOptionParser builder option
        value <- getMaybeValue values option
            |> Result.try

        when value is
            Err NoValue -> Ok Bool.false
            Ok (Err NoValue) -> Ok Bool.true
            Ok (Ok _val) -> Err (OptionDoesNotExpectValue option)

## Add a flag that can be given multiple times to your CLI builder.
##
## Parsing arguments will fail if this flag is ever given a value,
## e.g. `--flag=value`.
##
## ```roc
## expect
##     { parser } =
##         Cli.weave {
##             force: <- Opt.count { short: "f", long: "force" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "-f", "--force", "-fff"]
##     == SuccessfullyParsed { force: 5 }
## ```
count : OptionConfigParams -> (CliBuilder (U64 -> state) GetOptionsAction -> CliBuilder state action)
count = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: None, plurality: Many, short, long, help }

    \builder ->
        values <- updateBuilderWithOptionParser builder option

        if values |> List.any Result.isOk then
            Err (OptionDoesNotExpectValue option)
        else
            Ok (List.len values)
