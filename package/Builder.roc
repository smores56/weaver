interface Builder
    exposes [
        CliBuilder,
        cliBuilder,
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

GetOptionsAction : { getOptions : {} }
GetParamsAction : { getParams : {} }
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
        options: builder.options,
        parameters: builder.parameters,
        subcommands: HasSubcommands builder.subcommands,
    },
    parser: \args ->
        builder.parser args
        |> Result.map \(data, remainingArgs) ->
            (mapper data, remainingArgs),
}

finishCli :
    CliBuilder state action,
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
    # TODO: better formatting for crash
    when result is
        Ok cli -> cli
        Err err -> crash "$(Inspect.toStr err)"

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

singleOption : OptionConfig, (ArgValue -> Result a ArgExtractErr) -> (CliBuilder (a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
singleOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            parsedValue <- getSingleValue values option
                |> Result.try \val -> valueParser val
                |> Result.try

            Ok (data parsedValue, remainingArgs)

        updateBuilderWithOption builder newParser option

maybeOption : OptionConfig, (ArgValue -> Result a ArgExtractErr) -> (CliBuilder (Result a [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            parsedValue =
                when getOptionalValue values option is
                    Ok (Ok val) -> valueParser val |> Result.map Ok
                    Ok (Err NoValue) -> Ok (Err NoValue)
                    Err err -> Err err

            parsedValue
            |> Result.map \val ->
                (data val, remainingArgs)

        updateBuilderWithOption builder newParser option

listOption : OptionConfig, (ArgValue -> Result a ArgExtractErr) -> (CliBuilder (List a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
listOption = \option, valueParser ->
    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            parsedValues <- values
                |> List.mapTry \val -> valueParser val
                |> Result.try

            Ok (data parsedValues, remainingArgs)

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

numOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (I64 -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
numOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: One, argsNeeded: One, short, long, name, help }
    singleOption option (parseNumArgValue option)

strOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Str -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
strOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: One, argsNeeded: One, short, long, name, help }
    singleOption option (parseStrArgValue option)

customOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
customOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: One, argsNeeded: One, short, long, name, help }
    singleOption option (parseCustomArgValue option parser)

maybeNumOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Result I64 [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeNumOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: Optional, argsNeeded: One, short, long, name, help }
    maybeOption option (parseNumArgValue option)

maybeStrOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Result Str [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeStrOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: Optional, argsNeeded: One, short, long, name, help }
    maybeOption option (parseStrArgValue option)

maybeCustomOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (Result a [NoValue] -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
maybeCustomOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: Optional, argsNeeded: One, short, long, name, help }
    maybeOption option (parseCustomArgValue option parser)

numListOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (List I64 -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
numListOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: Many, argsNeeded: One, short, long, name, help }
    listOption option (parseNumArgValue option)

strListOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (List Str -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
strListOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: Many, argsNeeded: One, short, long, name, help }
    listOption option (parseStrArgValue option)

customListOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (List a -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
customListOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: Many, argsNeeded: One, short, long, name, help }
    listOption option (parseCustomArgValue option parser)

flagOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Bool -> state) GetOptionsAction -> CliBuilder state GetOptionsAction)
flagOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Bool, plurality: Optional, argsNeeded: Zero, short, long, name, help }

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

occurrenceOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (U64 -> state) GetOptionsAction -> CliBuilder state action)
occurrenceOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Bool, plurality: Many, argsNeeded: Zero, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            if values |> List.any Result.isOk then
                Err (OptionDoesNotExpectValue option)
            else
                Ok (data (List.len values), remainingArgs)

        updateBuilderWithOption builder newParser option

strParam : { name : Str, help ? Str } -> (CliBuilder (Str -> state) {}action -> CliBuilder state GetParamsAction)
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

strListParam : { name : Str, help ? Str } -> (CliBuilder (List Str -> state) {}action -> CliBuilder state [])
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
            e: <- numOption { short: "e" },
            g: <- numOption { short: "g" },
        }
        |> finishSubcommand { name: "sub-sub", description: "", mapper: SubSub }

    subcommandParser =
        cliBuilder {
            d: <- numOption { short: "d" },
            f: <- numOption { short: "f" },
            sc: <- subcommandField [subSubcommandParser],
        }
        |> finishSubcommand { name: "sub", description: "", mapper: Sub }

    { parser } =
        cliBuilder {
            alpha: <- numOption { short: "a" },
            beta: <- flagOption { short: "b", long: "beta" },
            xyz: <- strOption { long: "xyz" },
            verbosity: <- occurrenceOption { short: "v", long: "verbose" },
            sc: <- subcommandField [subcommandParser],
        }
        |> finishCli { name: "app" }
        |> assertCliIsValid

    out = parser ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv"]

    out == Ok { alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4, sc: Err NoSubcommand }

expect
    # subSubcommandParser1 =
    #     cliBuilder {
    #         a: <- numOption { short: "a" },
    #         b: <- numOption { short: "b" },
    #     }
    #     |> finishSubcommand { name: "ss1", description: "", mapper: SS1 }
    # subSubcommandParser2 =
    #     cliBuilder {
    #         a: <- numOption { short: "a" },
    #         c: <- numOption { short: "c" },
    #     }
    #     |> finishSubcommand { name: "ss2", description: "", mapper: SS2 }
    subcommandParser1 =
        cliBuilder {
            d: <- numOption { short: "d" },
            e: <- numOption { short: "e" },
            # sc: <- subcommandField [subSubcommandParser1, subSubcommandParser2],
        }
        |> finishSubcommand { name: "s1", description: "", mapper: S1 }

    subcommandParser2 =
        cliBuilder {
            d: <- numOption { short: "d" },
            f: <- numOption { short: "f" },
        }
        |> finishSubcommand { name: "s2", description: "", mapper: S2 }

    { parser, config: _ } =
        cliBuilder {
            x: <- numOption { short: "x" },
            sc: <- subcommandField [subcommandParser1, subcommandParser2],
            y: <- strParam { name: "y" },
            p: <- strListParam { name: "p" },
        }
        |> finishCli { name: "app" }
        |> assertCliIsValid

    out = parser ["app", "-x", "123", "y", "s1", "-d", "456", "-e", "789", "ss2", "-a", "135", "-c", "246"]

    # out == Ok { x: 123, y: "y", p: [], sc: Ok (S1 { sc: Ok (SS2 { a: 135, c: 246 }), d: 456, e: 789 }) }
    out == Ok { x: 123, y: "y", p: [], sc: Ok (S1 { d: 456, e: 789 }) }
