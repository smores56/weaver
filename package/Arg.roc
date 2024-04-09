interface Arg
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

num : OptionConfigParams -> (CliBuilder (I64 -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
num = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Num, plurality: One, short, long, help }
    singleOption option (parseNumArgValue option)

str : OptionConfigParams -> (CliBuilder (Str -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
str = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Str, plurality: One, short, long, help }
    singleOption option Ok

custom : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
custom = \{ short ? "", long ? "", help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: One, short, long, help }
    singleOption option (parseCustomArgValue option parser)

maybeNum : OptionConfigParams -> (CliBuilder (Result I64 [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeNum = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Num, plurality: Optional, short, long, help }
    maybeOption option (parseNumArgValue option)

maybeStr : OptionConfigParams -> (CliBuilder (Result Str [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeStr = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Str, plurality: Optional, short, long, help }
    maybeOption option Ok

maybeCustom : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (Result a [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeCustom = \{ short ? "", long ? "", help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: Optional, short, long, help }
    maybeOption option (parseCustomArgValue option parser)

numList : OptionConfigParams -> (CliBuilder (List I64 -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
numList = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Num, plurality: Many, short, long, help }
    listOption option (parseNumArgValue option)

strList : OptionConfigParams -> (CliBuilder (List Str -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
strList = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: Str, plurality: Many, short, long, help }
    listOption option Ok

customList : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (List a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
customList = \{ short ? "", long ? "", help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: Many, short, long, help }
    listOption option (parseCustomArgValue option parser)

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

count : OptionConfigParams -> (CliBuilder (U64 -> state) GetOptionsAction -> CliBuilder state action)
count = \{ short ? "", long ? "", help ? "" } ->
    option = { expectedType: None, plurality: Many, short, long, help }

    \builder ->
        values <- updateBuilderWithOptionParser builder option

        if values |> List.any Result.isOk then
            Err (OptionDoesNotExpectValue option)
        else
            Ok (List.len values)
