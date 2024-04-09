interface Cli
    exposes [
        CliParser,
        weave,
        finish,
        assertValid,
        parseOrDisplayMessage,
    ]
    imports [
        Base.{
            ArgParser,
            ArgParserResult,
            ArgExtractErr,
            OptionConfigParams,
            OptionConfig,
            ParameterConfigParams,
            ParameterConfig,
            CliConfig,
            CliConfigParams,
            SubcommandConfig,
            SubcommandsConfig,
            SubcommandConfigParams,
            mapSuccessfullyParsed,
        },
        Parser.{ Arg, parseArgs },
        Builder.{ CliBuilder, GetOptionsAction },
        Validate.{ validateCli, CliValidationErr },
        ErrorFormatter.{
            formatArgExtractErr,
            formatCliValidationErr,
        },
        Help.{ helpText, usageHelp },
    ]

CliParser state : { config : CliConfig, parser : List Str -> ArgParserResult state }

weave : base -> CliBuilder base GetOptionsAction
weave = \base -> Builder.fromState base

ensureAllArgsWereParsed : List Arg -> Result {} ArgExtractErr
ensureAllArgsWereParsed = \remainingArgs ->
    when remainingArgs is
        [] -> Ok {}
        [first, ..] ->
            extraArgErr =
                when first is
                    Parameter param -> ExtraParamProvided param
                    Long long -> UnrecognizedLongArg long.name
                    Short short -> UnrecognizedShortArg short
                    ShortGroup sg ->
                        firstShortArg = List.first sg.names |> Result.withDefault ""
                        UnrecognizedShortArg firstShortArg

            Err extraArgErr

finish : CliBuilder state action, CliConfigParams -> Result (CliParser state) CliValidationErr
finish = \builder, { name, authors ? [], version ? "", description ? "" } ->
    { options, parameters, subcommands, parser } =
        builder
        |> Builder.checkForHelpAndVersion
        |> Builder.updateParser \data ->
            ensureAllArgsWereParsed data.remainingArgs
            |> Result.map \{} -> data
        |> Builder.intoParts

    config = {
        name,
        authors,
        version,
        description,
        options,
        parameters,
        subcommands: HasSubcommands subcommands,
    }

    validateCli config
    |> Result.map \_ -> {
        config,
        parser: \args ->
            parser { args: parseArgs args, subcommandPath: [name] }
            |> mapSuccessfullyParsed \{ data } -> data,
    }

assertValid : Result (CliParser state) CliValidationErr -> CliParser state
assertValid = \result ->
    when result is
        Ok cli -> cli
        Err err -> crash (formatCliValidationErr err)

parseOrDisplayMessage : CliParser state, List Str -> Result state Str
parseOrDisplayMessage = \parser, args ->
    when parser.parser args is
        SuccessfullyParsed data -> Ok data
        # TODO: guess submodule so we can tell which subcommand was called even if help is parsed first
        ShowHelp { subcommandPath } -> Err (helpText { config: parser.config, subcommandPath })
        ShowVersion -> Err parser.config.version
        IncorrectUsage err { subcommandPath } ->
            usageStr = usageHelp parser.config subcommandPath
            incorrectUsageStr =
                """
                Error: $(formatArgExtractErr err)

                $(usageStr)
                """

            Err incorrectUsageStr

expect
    weave {}
    |> finish { name: "empty" }
    |> Result.isOk
