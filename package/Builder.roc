interface Builder
    exposes [
    ]
    imports [
        Config.{
            DataParser,
            ArgExtractErr,
            OptionConfigParams,
            OptionConfig,
            helpOption,
            versionOption,
            ParameterConfigParams,
            ParameterConfig,
            CliConfig,
            CliConfigParams,
            SubcommandConfig,
            SubcommandsConfig,
            SubcommandConfigParams,
        },
        Parser.{
            Arg,
            ArgValue,
            ParsedArgs,
            ArgParseErr,
            parseArgs,
        },
        Extract.{
            extractParamValues,
            extractOptionValues,
            getSingleValue,
            getOptionalValue,
        },
        Validate.{ validateCli, CliValidationErr },
        ErrorFormatter.{ formatCliValidationErr },
    ]

GetOptionsAction : { getOptions : {} }
GetParamsAction : { getParams : {} }
StopCollectingAction : []

CliParser state : { config : CliConfig, parser : List Str -> Result state ArgExtractErr }

CliBuilder state action := {
    parser : DataParser state,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : Dict Str SubcommandConfig,
}

cliBuilder : base -> CliBuilder base GetOptionsAction
cliBuilder = \base ->
    @CliBuilder {
        parser: \args -> Ok (base, args),
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

getValuesForOption : CliBuilder state action, OptionConfig, List Arg -> Result { data : state, values : List ArgValue, remainingArgs : List Arg } ArgExtractErr
getValuesForOption = \@CliBuilder builder, option, args ->
    (data, restOfArgs) <- builder.parser args
        |> Result.try
    { values, remainingArgs } <- extractOptionValues { args: restOfArgs, option }
        |> Result.try

    Ok { data, values, remainingArgs }

getValuesForParam : CliBuilder state action, ParameterConfig, List Arg -> Result { data : state, values : List Str, remainingArgs : List Arg } ArgExtractErr
getValuesForParam = \@CliBuilder builder, param, args ->
    (data, restOfArgs) <- builder.parser args
        |> Result.try
    { values, remainingArgs } <- extractParamValues { args: restOfArgs, param }
        |> Result.try

    Ok { data, values, remainingArgs }

updateBuilderWithOption : CliBuilder (a -> state) action1, DataParser state, OptionConfig -> CliBuilder state action2
updateBuilderWithOption = \@CliBuilder builder, parser, option ->
    @CliBuilder {
        parameters: builder.parameters,
        options: builder.options |> List.append option,
        subcommands: builder.subcommands,
        parser,
    }

updateBuilderWithParam : CliBuilder (a -> state) action1, DataParser state, ParameterConfig -> CliBuilder state action2
updateBuilderWithParam = \@CliBuilder builder, parser, param ->
    @CliBuilder {
        parameters: builder.parameters |> List.append param,
        options: builder.options,
        subcommands: builder.subcommands,
        parser,
    }

finishSubcommand : CliBuilder state action, { name : Str, description : Str, mapper : state -> commonState } -> { name : Str, parser : List Arg -> Result (commonState, List Arg) ArgExtractErr, config : SubcommandConfig }
finishSubcommand = \@CliBuilder builder, { name, description, mapper } -> {
    name,
    config: {
        description,
        options: builder.options |> List.concat [helpOption, versionOption],
        parameters: builder.parameters,
        subcommands: HasSubcommands builder.subcommands,
    },
    parser: \args ->
        builder.parser args
        |> Result.map \(data, remainingArgs) ->
            (mapper data, remainingArgs),
}

finishCli : CliBuilder state action, CliConfigParams -> Result { config : CliConfig, parser : List Str -> Result state ArgExtractErr } CliValidationErr
finishCli = \@CliBuilder builder, { name, authors ? [], version ? "", description ? "" } ->
    parser = \args ->
        parsedArgs <- parseArgs args
            |> Result.mapErr FailedToParseArgs
            |> Result.try

        (data, _remainingArgs) <- builder.parser parsedArgs
            |> Result.try

        # TODO allow ensuring no unknown args were passed
        Ok data

    config = {
        name,
        authors,
        version,
        description,
        subcommands: HasSubcommands builder.subcommands,
        options: builder.options,
        parameters: builder.parameters,
    }

    validateCli config
    |> Result.map \_ -> { config, parser }

assertCliIsValid : Result (CliParser state) CliValidationErr -> CliParser state
assertCliIsValid = \result ->
    when result is
        Err err -> crash (formatCliValidationErr err)
        Ok cli -> cli

subcommandField : List { name : Str, parser : DataParser subState, config : SubcommandConfig } -> (CliBuilder (Result subState [NoSubcommand] -> state) GetOptionsAction -> CliBuilder state GetParamsAction)
subcommandField = \subcommandConfigs ->
    subcommands =
        subcommandConfigs
        |> List.map \{ name, config } -> (name, config)
        |> Dict.fromList

    getParamArg = \arg ->
        when arg is
            Parameter p -> Ok p
            _other -> Err NotParameter

    \@CliBuilder builder ->
        # TODO: extract to new function to simplify
        newParser = \args ->
            (data, remainingArgs) <- builder.parser args
                |> Result.try

            firstParamResult =
                remainingArgs
                |> List.findFirstIndex \arg ->
                    Result.isOk (getParamArg arg)
                |> Result.try \argIndex ->
                    List.get args argIndex
                    |> Result.try getParamArg
                    |> Result.map \arg -> (arg, argIndex)

            when firstParamResult is
                Err _ ->
                    Ok (data (Err NoSubcommand), remainingArgs)

                Ok (param, paramIndex) ->
                    subcommand =
                        subcommandConfigs
                        |> List.keepIf \c -> c.name == param
                        |> List.first

                    when subcommand is
                        Err _ ->
                            Ok (data (Err NoSubcommand), remainingArgs)

                        Ok sc ->
                            parentRemainingArgs =
                                List.takeFirst remainingArgs paramIndex
                            subcommandArgs =
                                List.dropFirst remainingArgs (paramIndex + 1)
                            (subData, subRemainingArgs) <- sc.parser subcommandArgs
                                |> Result.try

                            Ok (data (Ok subData), List.concat parentRemainingArgs subRemainingArgs)

        @CliBuilder {
            options: builder.options,
            parameters: builder.parameters,
            parser: newParser,
            subcommands: subcommands,
        }

singleOption : OptionConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
singleOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            parsedValue <- getSingleValue values option
                |> Result.try \value ->
                    when value is
                        Ok val -> valueParser val
                        Err NoValue -> Err (NoValueProvidedForOption option)
                |> Result.try

            Ok (data parsedValue, remainingArgs)

        updateBuilderWithOption builder newParser option

maybeOption : OptionConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (Result a [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            parsedValue =
                when getOptionalValue values option is
                    Err err -> Err err
                    Ok (Err NoValue) -> Ok (Err NoValue)
                    Ok (Ok val) ->
                        when val is
                            Ok v -> valueParser v |> Result.map Ok
                            Err NoValue -> Err (NoValueProvidedForOption option)

            parsedValue
            |> Result.map \val ->
                (data val, remainingArgs)

        updateBuilderWithOption builder newParser option

listOption : OptionConfig, (Str -> Result a ArgExtractErr) -> (CliBuilder (List a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
listOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            parsedValues <- values
                |> List.mapTry \value ->
                    when value is
                        Ok val -> valueParser val
                        Err NoValue -> Err (NoValueProvidedForOption option)
                |> Result.try

            Ok (data parsedValues, remainingArgs)

        updateBuilderWithOption builder newParser option

parseStrArgValue : OptionConfig -> (Str -> Result Str ArgExtractErr)
parseStrArgValue = \_option -> \value -> Ok value

parseNumArgValue : OptionConfig -> (Str -> Result I64 ArgExtractErr)
parseNumArgValue = \option ->
    \value ->
        Str.toI64 value |> Result.mapErr \_ -> InvalidNumArg option

parseCustomArgValue : OptionConfig, (Str -> Result a [InvalidValue Str]) -> (Str -> Result a ArgExtractErr)
parseCustomArgValue = \option, parser ->
    \value ->
        parser value |> Result.mapErr \InvalidValue reason -> InvalidCustomArg option reason

numOption : OptionConfigParams -> (CliBuilder (I64 -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
numOption = \{ name, help ? "" } ->
    option = { expectedType: Num, plurality: One, name, help }
    singleOption option (parseNumArgValue option)

strOption : OptionConfigParams -> (CliBuilder (Str -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
strOption = \{ name, help ? "" } ->
    option = { expectedType: Str, plurality: One, name, help }
    singleOption option (parseStrArgValue option)

customOption : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
customOption = \{ name, help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: One, name, help }
    singleOption option (parseCustomArgValue option parser)

maybeNumOption : OptionConfigParams -> (CliBuilder (Result I64 [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeNumOption = \{ name, help ? "" } ->
    option = { expectedType: Num, plurality: Optional, name, help }
    maybeOption option (parseNumArgValue option)

maybeStrOption : OptionConfigParams -> (CliBuilder (Result Str [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeStrOption = \{ name, help ? "" } ->
    option = { expectedType: Str, plurality: Optional, name, help }
    maybeOption option (parseStrArgValue option)

maybeCustomOption : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (Result a [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeCustomOption = \{ name, help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: Optional, name, help }
    maybeOption option (parseCustomArgValue option parser)

numListOption : OptionConfigParams -> (CliBuilder (List I64 -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
numListOption = \{ name, help ? "" } ->
    option = { expectedType: Num, plurality: Many, name, help }
    listOption option (parseNumArgValue option)

strListOption : OptionConfigParams -> (CliBuilder (List Str -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
strListOption = \{ name, help ? "" } ->
    option = { expectedType: Str, plurality: Many, name, help }
    listOption option (parseStrArgValue option)

customListOption : { typeName : Str, parser : Str -> Result a [InvalidValue Str] }OptionConfigParams -> (CliBuilder (List a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
customListOption = \{ name, help ? "", typeName, parser } ->
    option = { expectedType: Custom typeName, plurality: Many, name, help }
    listOption option (parseCustomArgValue option parser)

flagOption : OptionConfigParams -> (CliBuilder (Bool -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
flagOption = \{ name, help ? "" } ->
    option = { expectedType: None, plurality: Optional, name, help }

    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getOptionalValue values option
                |> Result.try

            when value is
                Err NoValue -> Ok (data Bool.false, remainingArgs)
                Ok (Err NoValue) -> Ok (data Bool.true, remainingArgs)
                Ok (Ok _val) -> Err (OptionDoesNotExpectValue option)

        updateBuilderWithOption builder newParser option

occurrenceOption : OptionConfigParams -> (CliBuilder (U64 -> state) GetOptionsAction -> CliBuilder state action)
occurrenceOption = \{ name, help ? "" } ->
    option = { expectedType: None, plurality: Many, name, help }

    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            if values |> List.any Result.isOk then
                Err (OptionDoesNotExpectValue option)
            else
                Ok (data (List.len values), remainingArgs)

        updateBuilderWithOption builder newParser option

strParam : ParameterConfigParams -> (CliBuilder (Str -> state) {}action -> CliBuilder state GetParamsAction)
strParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: One }

    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForParam builder param args
                |> Result.try

            when List.first values is
                Ok single -> Ok (data single, remainingArgs)
                Err ListWasEmpty -> Err (MissingParam param)

        updateBuilderWithParam builder newParser param

maybeStrParam : { name : Str, help ? Str } -> (CliBuilder (ArgValue -> state) {}action -> CliBuilder state GetParamsAction)
maybeStrParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Optional }

    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForParam builder param args
                |> Result.try

            when List.first values is
                Ok single -> Ok (data (Ok single), remainingArgs)
                Err ListWasEmpty -> Ok (data (Err NoValue), remainingArgs)

        updateBuilderWithParam builder newParser param

strListParam : { name : Str, help ? Str } -> (CliBuilder (List Str -> state) {}action -> CliBuilder state StopCollectingAction)
strListParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Many }

    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForParam builder param args
                |> Result.try

            Ok (data values, remainingArgs)

        updateBuilderWithParam builder newParser param

expect
    subSubcommandParser =
        cliBuilder {
            e: <- numOption { name: Short "e" },
            g: <- numOption { name: Short "g" },
        }
        |> finishSubcommand { name: "sub-sub", description: "First subcommand", mapper: SubSub }

    subcommandParser =
        cliBuilder {
            d: <- numOption { name: Short "d" },
            f: <- numOption { name: Short "f" },
            sc: <- subcommandField [subSubcommandParser],
        }
        |> finishSubcommand { name: "sub", description: "Second subcommand", mapper: Sub }

    { parser } =
        cliBuilder {
            alpha: <- numOption { name: Short "a" },
            beta: <- flagOption { name: Both "b" "beta" },
            xyz: <- strOption { name: Long "xyz" },
            verbosity: <- occurrenceOption { name: Both "v" "verbose" },
            sc: <- subcommandField [subcommandParser],
        }
        |> finishCli { name: "app" }
        |> assertCliIsValid

    out = parser ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv"]

    out == Ok { alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4, sc: Err NoSubcommand }
