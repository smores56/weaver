interface Builder
    exposes [
        CliBuilder,
        cliBuilder,
        finishSubcommand,
        finishCli,
        assertCliIsValid,
        parseOrDisplayMessage,
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
            ArgParser,
            ArgParserResult,
            onSuccessfulArgParse,
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
        ErrorFormatter.{ formatArgExtractErr, formatCliValidationErr },
        Help.{ helpText, usageHelp },
    ]

GetOptionsAction : { getOptions : {} }
GetParamsAction : { getParams : {} }
StopCollectingAction : []

CliParser state : { config : CliConfig, parser : List Str -> ArgParserResult state }

CliBuilder state action := {
    parser : ArgParser state,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : Dict Str SubcommandConfig,
}

cliBuilder : base -> CliBuilder base GetOptionsAction
cliBuilder = \base ->
    @CliBuilder {
        parser: \{ args, subcommandPath } -> SuccessfullyParsed { data: base, remainingArgs: args, subcommandPath },
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

updateBuilderWithOptionParser : CliBuilder (a -> state) action, OptionConfig, (List ArgValue -> Result a ArgExtractErr) -> CliBuilder state nextAction
updateBuilderWithOptionParser = \@CliBuilder builder, option, valueParser ->
    parser =
        { data, values } <- buildParserUsingExtractedOption (@CliBuilder builder) option
        valueParser values |> Result.map data

    @CliBuilder {
        parameters: builder.parameters,
        options: builder.options |> List.append option,
        subcommands: builder.subcommands,
        parser,
    }

buildParserUsingExtractedOption : CliBuilder state action, OptionConfig, ({ data : state, values : List ArgValue } -> Result nextState ArgExtractErr) -> ArgParser nextState
buildParserUsingExtractedOption = \@CliBuilder builder, option, handler ->
    { data, remainingArgs: restOfArgs, subcommandPath } <- onSuccessfulArgParse builder.parser

    when extractOptionValues { args: restOfArgs, option } is
        Err extractErr -> IncorrectUsage extractErr { subcommandPath }
        Ok { values, remainingArgs, specialFlags } ->
            if specialFlags.help then
                ShowHelp { subcommandPath }
            else if specialFlags.version then
                ShowVersion
            else
                when handler { data, values } is
                    Ok nextData -> SuccessfullyParsed { data: nextData, remainingArgs, subcommandPath }
                    Err err -> IncorrectUsage err { subcommandPath }

updateBuilderWithParameterParser : CliBuilder (a -> state) action, ParameterConfig, (List Str -> Result a ArgExtractErr) -> CliBuilder state nextAction
updateBuilderWithParameterParser = \@CliBuilder builder, param, valueParser ->
    parser =
        { data, values } <- buildParserUsingExtractedParameter (@CliBuilder builder) param
        valueParser values |> Result.map data

    @CliBuilder {
        parameters: builder.parameters |> List.append param,
        options: builder.options,
        subcommands: builder.subcommands,
        parser,
    }

buildParserUsingExtractedParameter : CliBuilder state action, ParameterConfig, ({ data : state, values : List Str } -> Result nextState ArgExtractErr) -> ArgParser nextState
buildParserUsingExtractedParameter = \@CliBuilder builder, param, handler ->
    { data, remainingArgs: restOfArgs, subcommandPath } <- onSuccessfulArgParse builder.parser

    when extractParamValues { args: restOfArgs, param } is
        Err extractErr -> IncorrectUsage extractErr { subcommandPath }
        Ok { values, remainingArgs } ->
            when handler { data, values } is
                Ok nextData -> SuccessfullyParsed { data: nextData, remainingArgs, subcommandPath }
                Err err -> IncorrectUsage err { subcommandPath }

finishSubcommand : CliBuilder state action, { name : Str, description : Str, mapper : state -> commonState } -> { name : Str, parser : ArgParser commonState, config : SubcommandConfig }
finishSubcommand = \@CliBuilder builder, { name, description, mapper } -> {
    name,
    config: {
        description,
        options: builder.options |> List.concat [helpOption, versionOption],
        parameters: builder.parameters,
        subcommands: HasSubcommands builder.subcommands,
    },
    parser: onSuccessfulArgParse builder.parser \{ data, remainingArgs, subcommandPath } ->
        SuccessfullyParsed { data: mapper data, remainingArgs, subcommandPath },
}

finishCli : CliBuilder state action, CliConfigParams -> Result (CliParser state) CliValidationErr
finishCli = \@CliBuilder builder, { name, authors ? [], version ? "", description ? "" } ->
    parser =
        { data, remainingArgs, subcommandPath } <- onSuccessfulArgParse builder.parser
        when remainingArgs is
            [] -> SuccessfullyParsed { data, remainingArgs, subcommandPath }
            [first, ..] ->
                extraArgErr =
                    when first is
                        Parameter param -> ExtraParamProvided param
                        Long long -> UnrecognizedLongArg long.name
                        Short short -> UnrecognizedShortArg short
                        ShortGroup sg ->
                            firstShortArg = List.first sg.names |> Result.withDefault ""
                            UnrecognizedShortArg firstShortArg

                IncorrectUsage extraArgErr { subcommandPath }

    config = {
        name,
        authors,
        version,
        description,
        subcommands: HasSubcommands builder.subcommands,
        options: builder.options |> List.concat [helpOption, versionOption],
        parameters: builder.parameters,
    }

    validateCli config
    |> Result.map \_ -> {
        config,
        parser: \args ->
            when parser { args: parseArgs args, subcommandPath: [name] } is
                ShowVersion -> ShowVersion
                ShowHelp { subcommandPath } -> ShowHelp { subcommandPath }
                IncorrectUsage argExtractErr { subcommandPath } -> IncorrectUsage argExtractErr { subcommandPath }
                SuccessfullyParsed { data, remainingArgs: _, subcommandPath: _ } ->
                    SuccessfullyParsed data
    }

assertCliIsValid : Result (CliParser state) CliValidationErr -> CliParser state
assertCliIsValid = \result ->
    when result is
        Err err -> crash (formatCliValidationErr err)
        Ok cli -> cli

# TODO: make a module for this and other helpers
parseOrDisplayMessage : CliParser state, List Str -> Result state Str
parseOrDisplayMessage = \parser, args ->
    when parser.parser args is
        SuccessfullyParsed data -> Ok data
        # TODO: guess submodule so we can tell which subcommand was called even if help is parsed first
        ShowHelp { subcommandPath } -> Err (helpText { config: parser.config, subcommandPath })
        ShowVersion -> Err parser.config.version
        IncorrectUsage err { subcommandPath } ->
            # TODO: pass subcommandPath to usageHelp
            usageStr = usageHelp parser.config subcommandPath
            incorrectUsageStr =
                """
                Error: $(formatArgExtractErr err)

                $(usageStr)
                """

            Err incorrectUsageStr


subcommandField : List { name : Str, parser : ArgParser subState, config : SubcommandConfig } -> (CliBuilder (Result subState [NoSubcommand] -> state) GetOptionsAction -> CliBuilder state GetParamsAction)
subcommandField = \subcommandConfigs ->
    subcommands =
        subcommandConfigs
        |> List.map \{ name, config } -> (name, config)
        |> Dict.fromList

    \@CliBuilder builder ->
        # TODO: extract to new function to simplify
        newParser =
            { data, remainingArgs, subcommandPath } <- onSuccessfulArgParse builder.parser

            when remainingArgs is
                [] -> SuccessfullyParsed { data: data (Err NoSubcommand), remainingArgs, subcommandPath }
                [firstArg, .. as restOfArgs] ->
                    when firstArg is
                        Short short -> IncorrectUsage (UnrecognizedShortArg short) { subcommandPath }
                        Long long -> IncorrectUsage (UnrecognizedLongArg long.name) { subcommandPath }
                        ShortGroup sg -> IncorrectUsage (UnrecognizedShortArg (sg.names |> List.first |> Result.withDefault "")) { subcommandPath }
                        Parameter p ->
                            subcommandFound =
                                subcommandConfigs
                                |> List.findFirst \subConfig -> subConfig.name == p

                            when subcommandFound is
                                Err NotFound ->
                                    SuccessfullyParsed { data: data (Err NoSubcommand), remainingArgs, subcommandPath }

                                Ok subcommand ->
                                    subParser =
                                        { data: subData, remainingArgs: subRemainingArgs, subcommandPath: subSubcommandPath } <- onSuccessfulArgParse subcommand.parser
                                        SuccessfullyParsed { data: data (Ok subData), remainingArgs: subRemainingArgs, subcommandPath: subSubcommandPath }

                                    subParser { args: restOfArgs, subcommandPath: subcommandPath |> List.append p }

        @CliBuilder {
            parameters: builder.parameters,
            options: builder.options,
            parser: newParser,
            subcommands,
        }

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
        value <- getOptionalValue values option
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
        values <- updateBuilderWithOptionParser builder option
        value <- getOptionalValue values option
            |> Result.try

        when value is
            Err NoValue -> Ok Bool.false
            Ok (Err NoValue) -> Ok Bool.true
            Ok (Ok _val) -> Err (OptionDoesNotExpectValue option)

occurrenceOption : OptionConfigParams -> (CliBuilder (U64 -> state) GetOptionsAction -> CliBuilder state action)
occurrenceOption = \{ name, help ? "" } ->
    option = { expectedType: None, plurality: Many, name, help }

    \builder ->
        values <- updateBuilderWithOptionParser builder option

        if values |> List.any Result.isOk then
            Err (OptionDoesNotExpectValue option)
        else
            Ok (List.len values)

strParam : ParameterConfigParams -> (CliBuilder (Str -> state) {}action -> CliBuilder state GetParamsAction)
strParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: One }

    \builder ->
        values <- updateBuilderWithParameterParser builder param
        when List.first values is
            Ok single -> Ok (single)
            Err ListWasEmpty -> Err (MissingParam param)

maybeStrParam : { name : Str, help ? Str } -> (CliBuilder (ArgValue -> state) {}action -> CliBuilder state GetParamsAction)
maybeStrParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Optional }

    \builder ->
        values <- updateBuilderWithParameterParser builder param

        List.first values
        |> Result.mapErr \ListWasEmpty -> NoValue
        |> Ok

strListParam : { name : Str, help ? Str } -> (CliBuilder (List Str -> state) {}action -> CliBuilder state StopCollectingAction)
strListParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: Many }

    \builder ->
        values <- updateBuilderWithParameterParser builder param

        Ok values

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

    cliParser =
        cliBuilder {
            alpha: <- numOption { name: Short "a" },
            beta: <- flagOption { name: Both "b" "beta" },
            xyz: <- strOption { name: Long "xyz" },
            verbosity: <- occurrenceOption { name: Both "v" "verbose" },
            sc: <- subcommandField [subcommandParser],
        }
        |> finishCli { name: "app" }
        |> assertCliIsValid

    args = ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv"]

    out = cliParser.parser args

    out == SuccessfullyParsed { alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4, sc: Err NoSubcommand }
