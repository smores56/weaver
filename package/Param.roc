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

str : ParameterConfigParams -> (CliBuilder (Str -> state) {}action -> CliBuilder state GetParamsAction)
str = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: One }
    singleParam param Ok

maybeStr : ParameterConfigParams -> (CliBuilder (ArgValue -> state) {}action -> CliBuilder state GetParamsAction)
maybeStr = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Optional }
    maybeParam param Ok

strList : ParameterConfigParams -> (CliBuilder (List Str -> state) {}action -> CliBuilder state StopCollectingAction)
strList = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Many }
    listParam param Ok

num : ParameterConfigParams -> (CliBuilder (I64 -> state) {}action -> CliBuilder state GetParamsAction)
num = \{ name, help ? "" } ->
    param = { name, help, type: Num, plurality: One }
    singleParam param (parseNumParam param)

maybeNum : ParameterConfigParams -> (CliBuilder (Result I64 [NoValue] -> state) {}action -> CliBuilder state GetParamsAction)
maybeNum = \{ name, help ? "" } ->
    param = { name, help, type: Num, plurality: Optional }
    maybeParam param (parseNumParam param)

numList : ParameterConfigParams -> (CliBuilder (List I64 -> state) {}action -> CliBuilder state StopCollectingAction)
numList = \{ name, help ? "" } ->
    param = { name, help, type: Num, plurality: Many }
    listParam param (parseNumParam param)

custom : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }ParameterConfigParams -> (CliBuilder (a -> state) {}action -> CliBuilder state GetParamsAction)
custom = \{ name, help ? "", typeName, parser } ->
    param = { name, help, type: Custom typeName, plurality: One }
    singleParam param (parseCustomParam param parser)

maybeCustom : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }ParameterConfigParams -> (CliBuilder (Result a [NoValue] -> state) {}action -> CliBuilder state GetParamsAction)
maybeCustom = \{ name, help ? "", typeName, parser } ->
    param = { name, help, type: Custom typeName, plurality: Optional }
    maybeParam param (parseCustomParam param parser)

customList : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }ParameterConfigParams -> (CliBuilder (List a -> state) {}action -> CliBuilder state StopCollectingAction)
customList = \{ name, help ? "", typeName, parser } ->
    param = { name, help, type: Custom typeName, plurality: Many }
    listParam param (parseCustomParam param parser)
