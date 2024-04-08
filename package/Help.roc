interface Help
    exposes [helpText]
    imports [
        Config.{
            CliConfig,
            OptionConfig,
            ParameterConfig,
            SubcommandConfig,
            SubcommandsConfig,
        },
        Utils.{ toUpperCase, strLen },
    ]

helpText : { config : CliConfig, subcommandPath ? List Str } -> Str
helpText = \{ config, subcommandPath ? [] } ->
    findSubcommand : SubcommandConfig, List Str -> Result SubcommandConfig [KeyNotFound]
    findSubcommand = \command, path ->
        when path is
            [] -> Ok command
            [first, .. as rest] ->
                when command.subcommands is
                    NoSubcommands -> Err KeyNotFound
                    HasSubcommands scs ->
                        Dict.get scs first
                        |> Result.try \sc ->
                            findSubcommand sc rest

    baseCommand = {
        description: config.description,
        options: config.options,
        parameters: config.parameters,
        subcommands: config.subcommands,
    }

    (commandFound, subcommandPathFound) =
        when findSubcommand baseCommand (List.dropFirst subcommandPath 1) is
            Err KeyNotFound -> (baseCommand, [config.name])
            Ok c ->
                (
                    c,
                    if List.isEmpty subcommandPath then
                        [config.name]
                    else
                        subcommandPath,
                )

    helpTextForCommand {
        subcommandPath: subcommandPathFound,
        version: config.version,
        authors: config.authors,
        description: commandFound.description,
        options: commandFound.options,
        parameters: commandFound.parameters,
        subcommands: commandFound.subcommands,
    }

helpTextForCommand :
    {
        subcommandPath : List Str,
        version : Str,
        authors : List Str,
        description : Str,
        options : List OptionConfig,
        parameters : List ParameterConfig,
        subcommands : SubcommandsConfig,
    }
    -> Str
helpTextForCommand = \{ subcommandPath, version, authors, description, options, parameters, subcommands } ->
    name = subcommandPath |> Str.joinWith " "

    authorsText =
        if List.isEmpty authors then
            ""
        else
            "$(Str.joinWith authors " ")\n"

    descriptionText =
        if description != "" then
            description
        else
            "No description."

    subcommandsText =
        when subcommands is
            HasSubcommands scs if !(Dict.isEmpty scs) ->
                commandsHelp subcommands

            _noSubcommands -> ""

    parametersText =
        if List.isEmpty parameters then
            ""
        else
            parametersHelp parameters

    optionsText =
        if List.isEmpty options then
            ""
        else
            optionsHelp options

    """
    $(name) $(version)
    $(authorsText)
    $(descriptionText)

    $(usageHelp { name, options, parameters, subcommands })

    $([subcommandsText, parametersText, optionsText] |> Str.joinWith "\n\n")
    """

# TODO: consider showing required arguments in the usage
usageHelp :
    {
        name : Str,
        options : List OptionConfig,
        parameters : List ParameterConfig,
        subcommands : SubcommandsConfig,
    }
    -> Str
usageHelp = \{ name, options, parameters, subcommands } ->
    optionsStr =
        if List.isEmpty options then
            ""
        else
            " [OPTIONS]"

    paramsStr =
        parameters
        |> List.map \{ name: paramName, plurality } ->
            ellipsis =
                when plurality is
                    Optional | One -> ""
                    Many -> "..."

            " [$(paramName)]$(ellipsis)"
        |> Str.joinWith ""

    subcommandUsage =
        when subcommands is
            HasSubcommands sc if !(Dict.isEmpty sc) -> "  $(name) <COMMAND>"
            _other -> ""

    """
    Usage:
      $(name)$(optionsStr)$(paramsStr)
    $(subcommandUsage)
    """

commandsHelp : SubcommandsConfig -> Str
commandsHelp = \subcommands ->
    commands =
        when subcommands is
            NoSubcommands -> []
            HasSubcommands sc -> Dict.toList sc

    alignedCommands =
        commands
        |> List.map \(name, subConfig) ->
            (name, subConfig.description)
        |> alignTwoColumns

    """
    Commands:
    $(Str.joinWith alignedCommands "\n")
    """

parametersHelp : List ParameterConfig -> Str
parametersHelp = \params ->
    formattedParams =
        params
        |> List.map \param ->
            ellipsis =
                when param.plurality is
                    Optional | One -> ""
                    Many -> "..."

            ("[$(param.name)]$(ellipsis)", param.help)
        |> alignTwoColumns

    """
    Arguments:
    $(Str.joinWith formattedParams "\n")
    """

optionsHelp : List OptionConfig -> Str
optionsHelp = \options ->
    optionNameFormatter = \opt ->
        name =
            when opt.name is
                Short short -> "-$(short)"
                Long long -> "    --$(long)"
                Both short long -> "-$(short), --$(long)"

        typeName =
            when opt.expectedType is
                None -> ""
                Str -> " STR"
                Num -> " NUM"
                Custom c -> " $(toUpperCase c)"

        Str.concat name typeName

    formattedOptions =
        options
        |> List.map \option ->
            (optionNameFormatter option, option.help)
        |> alignTwoColumns

    """
    Options:
    $(Str.joinWith formattedOptions "\n")
    """

alignTwoColumns : List (Str, Str) -> List Str
alignTwoColumns = \columns ->
    maxFirstColumnLen =
        columns
        |> List.map \(first, _second) -> strLen first
        |> List.max
        |> Result.withDefault 0

    List.map columns \(first, second) ->
        buffer = Str.repeat " " (maxFirstColumnLen - strLen first)
        "  $(first)$(buffer)  $(second)"
