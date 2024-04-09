interface Subcommand
    exposes [finish, field]
    imports [
        Base.{
            ArgParser,
            ArgParserState,
            ArgParserResult,
            onSuccessfulArgParse,
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
        },
        Builder.{
            CliBuilder,
            GetOptionsAction,
            GetParamsAction,
        },
    ]

finish : CliBuilder state action, { name : Str, description : Str, mapper : state -> commonState } -> { name : Str, parser : ArgParser commonState, config : SubcommandConfig }
finish = \builder, { name, description, mapper } ->
    { options, parameters, subcommands, parser } =
        builder
        |> Builder.checkForHelpAndVersion
        |> Builder.updateParser \{ data, remainingArgs } ->
            Ok { data: mapper data, remainingArgs }
        |> Builder.intoParts

    config = {
        description,
        options,
        parameters,
        subcommands: HasSubcommands subcommands,
    }

    { name, config, parser }

getFirstArgToCheckForSubcommandCall : ArgParserState (Result state [NoSubcommand] -> subState), (Str -> ArgParserResult (ArgParserState subState)) -> ArgParserResult (ArgParserState subState)
getFirstArgToCheckForSubcommandCall = \{ data, remainingArgs, subcommandPath }, callback ->
    when List.first remainingArgs is
        Err ListWasEmpty -> SuccessfullyParsed { data: data (Err NoSubcommand), remainingArgs, subcommandPath }
        Ok firstArg ->
            when firstArg is
                Short short -> IncorrectUsage (UnrecognizedShortArg short) { subcommandPath }
                Long long -> IncorrectUsage (UnrecognizedLongArg long.name) { subcommandPath }
                ShortGroup sg -> IncorrectUsage (UnrecognizedShortArg (sg.names |> List.first |> Result.withDefault "")) { subcommandPath }
                Parameter p -> callback p

field : List { name : Str, parser : ArgParser subState, config : SubcommandConfig } -> (CliBuilder (Result subState [NoSubcommand] -> state) GetOptionsAction -> CliBuilder state GetParamsAction)
field = \subcommandConfigs ->
    subcommands =
        subcommandConfigs
        |> List.map \{ name, config } -> (name, config)
        |> Dict.fromList

    \builder ->
        builder
        |> Builder.addSubcommands subcommands
        |> Builder.bindParser \{ data, remainingArgs, subcommandPath } ->
            firstParam <- getFirstArgToCheckForSubcommandCall { data, remainingArgs, subcommandPath }
            subcommandFound =
                subcommandConfigs
                |> List.findFirst \subConfig -> subConfig.name == firstParam

            when subcommandFound is
                Err NotFound ->
                    SuccessfullyParsed { data: data (Err NoSubcommand), remainingArgs, subcommandPath }

                Ok subcommand ->
                    subParser =
                        { data: subData, remainingArgs: subRemainingArgs, subcommandPath: subSubcommandPath } <- onSuccessfulArgParse subcommand.parser
                        SuccessfullyParsed { data: data (Ok subData), remainingArgs: subRemainingArgs, subcommandPath: subSubcommandPath }

                    subParser {
                        args: List.dropFirst remainingArgs 1,
                        subcommandPath: subcommandPath |> List.append firstParam,
                    }
