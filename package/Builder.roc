interface Builder
    exposes [
        CliBuilder,
        cliBuilder,
        cliBuilderWithSubcommands,
        finishSubcommand,
        finishCli,
        assertCliIsValid,
        subcommandField,
        strOption,
        numOption,
        customOption,
        maybeStrOption,
        maybeNumOption,
        maybeCustomOption,
        strListOption,
        numListOption,
        customListOption,
        flagOption,
        occurrenceOption,
        strParam,
        maybeStrParam,
        strListParam,
    ]
    imports [
        Config.{
            DataParser,
            mapDataParserData,
            ArgExtractErr,
            OptionConfig,
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
    ]

GetSubcommandsAction : { getSubcommands : {} }
GetOptionsAction : { getOptions : {} }
GetParamsAction : { getParams : {} }
CliParser state : { config : CliConfig, parser : List Str -> Result state ArgExtractErr }

CliBuilder state subState subSubState action := {
    parser : DataParser state subState,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : Dict Str { parser : DataParser subState subSubState, config : SubcommandConfig },
}

cliBuilder : base -> CliBuilder base subState subSubState GetOptionsAction
cliBuilder = \base ->
    @CliBuilder {
        parser: \args -> Ok ({ data: base, subcommand: Err NoSubcommand }, args),
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

cliBuilderWithSubcommands : base -> CliBuilder base subState subSubState GetSubcommandsAction
cliBuilderWithSubcommands = \base ->
    @CliBuilder {
        parser: \args -> Ok ({ data: base, subcommand: Err NoSubcommand }, args),
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

getValuesForOption :
    CliBuilder state subState subSubState action,
    OptionConfig,
    List Arg
    -> Result
        {
            data : state,
            subcommand : Result subState [NoSubcommand],
            values : List ArgValue,
            remainingArgs : List Arg,
        }
        ArgExtractErr
getValuesForOption = \@CliBuilder builder, option, args ->
    ({ data, subcommand }, restOfArgs) <- builder.parser args
        |> Result.try

    when subcommand is
        Ok sc ->
            { values, remainingArgs, subcommandFound: _ } <- extractOptionValues {
                    args: restOfArgs,
                    option,
                    subcommands: Dict.empty {},
                }
                |> Result.try

            Ok { data, subcommand: Ok sc, values, remainingArgs }

        Err NoSubcommand ->
            { values, remainingArgs, subcommandFound } <- extractOptionValues {
                    args: restOfArgs,
                    option,
                    subcommands: builder.subcommands,
                }
                |> Result.try

            # TODO: what about child data?
            when subcommandFound is
                Ok found ->
                    # TODO: probably have to drop the first argument here, since it represents the name of the subcommand
                    ({ data: subcommandData, subcommand: _ }, otherArgs) <- found.parser found.args
                        |> Result.try

                    Ok { data, subcommand: Ok subcommandData, values, remainingArgs: otherArgs }

                Err NoSubcommand ->
                    Ok { data, subcommand: Err NoSubcommand, values, remainingArgs }

getValuesForParam :
    CliBuilder state subState subSubState action,
    ParameterConfig,
    List Arg
    -> Result
        {
            data : state,
            subcommand : Result subState [NoSubcommand],
            values : List Str,
            remainingArgs : List Arg,
        }
        ArgExtractErr
getValuesForParam = \@CliBuilder builder, param, args ->
    ({ data, subcommand }, restOfArgs) <- builder.parser args
        |> Result.try

    when subcommand is
        Ok sc ->
            { values, remainingArgs, subcommandFound: _ } <- extractParamValues {
                    args: restOfArgs,
                    param,
                    subcommands: Dict.empty {},
                }
                |> Result.try

            Ok { data, subcommand: Ok sc, values, remainingArgs }

        Err NoSubcommand ->
            { values, remainingArgs, subcommandFound } <- extractParamValues {
                    args: restOfArgs,
                    param,
                    subcommands: builder.subcommands,
                }
                |> Result.try

            # TODO: what about child data?
            when subcommandFound is
                Ok found ->
                    # TODO: probably have to drop the first argument here, since it represents the name of the subcommand
                    ({ data: subcommandData, subcommand: _ }, otherArgs) <- found.parser found.args
                        |> Result.try

                    Ok { data, subcommand: Ok subcommandData, values, remainingArgs: otherArgs }

                Err NoSubcommand ->
                    Ok { data, subcommand: Err NoSubcommand, values, remainingArgs }

updateBuilderWithOption : CliBuilder (a -> state) subState subSubState action1, DataParser state subState, OptionConfig -> CliBuilder state subState subSubState action2
updateBuilderWithOption = \@CliBuilder builder, parser, option ->
    @CliBuilder {
        parameters: builder.parameters,
        options: builder.options |> List.append option,
        subcommands: builder.subcommands,
        parser,
    }

updateBuilderWithParam : CliBuilder (a -> state) subState subSubState action1, DataParser state subState, ParameterConfig -> CliBuilder state subState subSubState action2
updateBuilderWithParam = \@CliBuilder builder, parser, param ->
    @CliBuilder {
        parameters: builder.parameters |> List.append param,
        options: builder.options,
        subcommands: builder.subcommands,
        parser,
    }

finishSubcommand : CliBuilder state subState subSubState action, { name : Str, description : Str, mapper : state -> newState } -> { name : Str, parser : DataParser newState subState, config : SubcommandConfig }
finishSubcommand = \@CliBuilder builder, { name, description, mapper } ->
    subcommands =
        builder.subcommands
        |> Dict.map \_n, sc -> sc.config
        |> HasSubcommands

    {
        name,
        config: {
            description,
            options: builder.options,
            parameters: builder.parameters,
            subcommands,
        },
        parser: mapDataParserData builder.parser mapper,
    }

finishCli :
    CliBuilder state subState subSubState action,
    {
        name ? Str,
        authors ? List Str,
        version ? Str,
        description ? Str,
    }
    -> Result
        {
            config : CliConfig,
            parser : List Str -> Result state ArgExtractErr,
        }
        CliValidationErr
finishCli = \@CliBuilder builder, { name ? "", authors ? [], version ? "", description ? "" } ->
    parser = \args ->
        parsedArgs <- parseArgs args
            |> Result.mapErr FailedToParseArgs
            |> Result.try
        ({ data, subcommand: _ }, _remainingArgs) <- builder.parser parsedArgs
            |> Result.try

        # TODO allow ensuring no unknown args were passed
        Ok data

    subcommands =
        builder.subcommands
        |> Dict.map \_n, sc -> sc.config
        |> HasSubcommands

    config = {
        name,
        authors,
        version,
        description,
        subcommands,
        options: builder.options,
        parameters: builder.parameters,
    }

    validateCli config
    |> Result.map \_ -> { config, parser }

assertCliIsValid : Result (CliParser state) CliValidationErr -> CliParser state
assertCliIsValid = \result ->
    # TODO: better formatting for crash
    when result is
        Ok cli -> cli
        Err err -> crash "$(Inspect.toStr err)"

subcommandField : List { name : Str, parser : DataParser subState subSubState, config : SubcommandConfig } -> (CliBuilder (Result subState [NoSubcommand] -> state) subState subSubState GetSubcommandsAction -> CliBuilder state subState subSubState GetOptionsAction)
subcommandField = \subcommandConfigs ->
    subcommands =
        subcommandConfigs
        |> List.map \{ name, parser, config } -> (name, { parser, config })
        |> Dict.fromList

    \@CliBuilder builder ->
        # TODO: rewrite this to make sure it works?
        newParser = \args ->
            when builder.parser args is
                Ok ({ data, subcommand }, remainingArgs) ->
                    Ok ({ data: data subcommand, subcommand }, remainingArgs)

                Err err -> Err err

        @CliBuilder {
            options: builder.options,
            parameters: builder.parameters,
            parser: newParser,
            subcommands: subcommands,
        }

singleOption : OptionConfig, (ArgValue -> Result a ArgExtractErr) -> (CliBuilder (a -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
singleOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            parsedValue <- getSingleValue values option
                |> Result.try \val -> valueParser val
                |> Result.try

            Ok ({ data: data parsedValue, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

maybeOption : OptionConfig, (ArgValue -> Result a ArgExtractErr) -> (CliBuilder (Result a [NoValue] -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
maybeOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            parsedValue =
                when getOptionalValue values option is
                    Ok (Ok val) -> valueParser val |> Result.map Ok
                    Ok (Err NoValue) -> Ok (Err NoValue)
                    Err err -> Err err

            parsedValue
            |> Result.map \val ->
                ({ data: data val, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

listOption : OptionConfig, (ArgValue -> Result a ArgExtractErr) -> (CliBuilder (List a -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
listOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            parsedValues <- values
                |> List.mapTry \val -> valueParser val
                |> Result.try

            Ok ({ data: data parsedValues, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

parseStrArgValue : OptionConfig -> (ArgValue -> Result Str ArgExtractErr)
parseStrArgValue = \option ->
    \value -> value |> Result.mapErr \NoValue -> NoValueProvidedForOption option

parseNumArgValue : OptionConfig -> (ArgValue -> Result I64 ArgExtractErr)
parseNumArgValue = \option ->
    \value ->
        when value is
            Ok val -> Str.toI64 val |> Result.mapErr \_ -> InvalidNumArg option
            Err NoValue -> Err (NoValueProvidedForOption option)

parseCustomArgValue : OptionConfig, (Str -> Result a [InvalidValue Str]) -> (ArgValue -> Result a ArgExtractErr)
parseCustomArgValue = \option, parser ->
    \value ->
        when value is
            Ok val -> parser val |> Result.mapErr \InvalidValue reason -> InvalidCustomArg option reason
            Err NoValue -> Err (NoValueProvidedForOption option)

numOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (I64 -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
numOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: One, argsNeeded: One, short, long, name, help }
    singleOption option (parseNumArgValue option)

strOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Str -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
strOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: One, argsNeeded: One, short, long, name, help }
    singleOption option (parseStrArgValue option)

customOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (a -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
customOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: One, argsNeeded: One, short, long, name, help }
    singleOption option (parseCustomArgValue option parser)

maybeNumOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Result I64 [NoValue] -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
maybeNumOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: Optional, argsNeeded: One, short, long, name, help }
    maybeOption option (parseNumArgValue option)

maybeStrOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Result Str [NoValue] -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
maybeStrOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: Optional, argsNeeded: One, short, long, name, help }
    maybeOption option (parseStrArgValue option)

maybeCustomOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (Result a [NoValue] -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
maybeCustomOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: Optional, argsNeeded: One, short, long, name, help }
    maybeOption option (parseCustomArgValue option parser)

numListOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (List I64 -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
numListOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: Many, argsNeeded: One, short, long, name, help }
    listOption option (parseNumArgValue option)

strListOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (List Str -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
strListOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: Many, argsNeeded: One, short, long, name, help }
    listOption option (parseStrArgValue option)

customListOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (List a -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
customListOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: Many, argsNeeded: One, short, long, name, help }
    listOption option (parseCustomArgValue option parser)

flagOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Bool -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState GetOptionsAction)
flagOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Bool, plurality: Optional, argsNeeded: Zero, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getOptionalValue values option
                |> Result.try

            when value is
                Err NoValue -> Ok ({ data: data Bool.false, subcommand }, remainingArgs)
                Ok (Err NoValue) -> Ok ({ data: data Bool.true, subcommand }, remainingArgs)
                Ok (Ok _val) -> Err (OptionDoesNotExpectValue option)

        updateBuilderWithOption builder newParser option

occurrenceOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (U64 -> state) subState subSubState GetOptionsAction -> CliBuilder state subState subSubState action)
occurrenceOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Bool, plurality: Many, argsNeeded: Zero, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            if values |> List.any Result.isOk then
                Err (OptionDoesNotExpectValue option)
            else
                Ok ({ data: data (List.len values), subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

strParam : { name : Str, help ? Str } -> (CliBuilder (Str -> state) subState subSubState {}action -> CliBuilder state subState subSubState GetParamsAction)
strParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: One }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForParam builder param args
                |> Result.try

            when List.first values is
                Ok single -> Ok ({ data: data single, subcommand }, remainingArgs)
                Err ListWasEmpty -> Err (MissingParam param)

        updateBuilderWithParam builder newParser param

maybeStrParam : { name : Str, help ? Str } -> (CliBuilder (ArgValue -> state) subState subSubState {}action -> CliBuilder state subState subSubState GetParamsAction)
maybeStrParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Optional }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForParam builder param args
                |> Result.try

            when List.first values is
                Ok single -> Ok ({ data: data (Ok single), subcommand }, remainingArgs)
                Err ListWasEmpty -> Ok ({ data: data (Err NoValue), subcommand }, remainingArgs)

        updateBuilderWithParam builder newParser param

strListParam : { name : Str, help ? Str } -> (CliBuilder (List Str -> state) subState subSubState {}action -> CliBuilder state subState subSubState [])
strListParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Many }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForParam builder param args
                |> Result.try

            Ok ({ data: data values, subcommand }, remainingArgs)

        updateBuilderWithParam builder newParser param

expect
    { parser, config: _ } =
        cliBuilder {
            alpha: <- numOption { short: "a" },
            beta: <- flagOption { short: "b", long: "--beta" },
            xyz: <- strOption { long: "xyz" },
            verbosity: <- occurrenceOption { short: "v", long: "--verbose" },
        }
        |> finishCli { name: "app" }
        |> assertCliIsValid

    out = parser ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv"]

    out == Ok { alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4 }

expect
    # subSubcommandParser1 =
    #     cliBuilder {
    #         a: <- numOption { short: "a" },
    #         b: <- numOption { short: "b" },
    #     }
    #     |> finishSubcommand { name: "ss1", description: "", mapper: SS1 }
    #
    # subSubcommandParser2 =
    #     cliBuilder {
    #         a: <- numOption { short: "a" },
    #         c: <- numOption { short: "c" },
    #     }
    #     |> finishSubcommand { name: "ss2", description: "", mapper: SS2 }
    #
    subcommandParser1 =
        cliBuilder {
            # sc: <- subcommandField [subSubcommandParser1, subSubcommandParser2],
            d: <- numOption { short: "d" },
            e: <- numOption { short: "e" },
        }
        |> finishSubcommand { name: "s1", description: "", mapper: S1 }

    subcommandParser2 =
        cliBuilder {
            d: <- numOption { short: "d" },
            f: <- numOption { short: "f" },
        }
        |> finishSubcommand { name: "s2", description: "", mapper: S2 }

    { parser, config: _ } =
        cliBuilderWithSubcommands {
            sc: <- subcommandField [subcommandParser1, subcommandParser2],
            x: <- numOption { short: "x" },
            y: <- strParam { name: "y" },
            p: <- strListParam { name: "p" },
        }
        |> finishCli { name: "app" }
        |> assertCliIsValid

    out = parser ["app", "-x", "123", "y", "s1", "-d", "456", "-e", "789", "ss2", "-a", "135", "-c", "246"]

    # out == Ok { x: 123, y: "y", sc: Ok (S1 { sc: Ok (SS2 { a: 135, c: 246 }), d: 456, e: 789 }) }

    out == Ok { x: 123, y: "y", sc: Ok (S1 { d: 456, e: 789 }), p: ["ss2"] }
