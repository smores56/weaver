interface Help
    exposes [helpText, usageHelp]
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

findSubcommandOrDefault : CliConfig, List Str -> { config : CliConfig, path : List Str }
findSubcommandOrDefault = \config, path ->
    baseCommand = {
        description: config.description,
        options: config.options,
        parameters: config.parameters,
        subcommands: config.subcommands,
    }

    when findSubcommand baseCommand (List.dropFirst path 1) is
        Err KeyNotFound -> { config, path }
        Ok { config: c, path: subPath } ->
            {
                config: {
                    name: config.name,
                    version: config.version,
                    authors: config.authors,
                    description: c.description,
                    options: c.options,
                    parameters: c.parameters,
                    subcommands: c.subcommands,
                },
                path: subPath,
            }

findSubcommand : SubcommandConfig, List Str -> Result { config : SubcommandConfig, path : List Str } [KeyNotFound]
findSubcommand = \command, path ->
    when path is
        [] -> Ok { config: command, path }
        [first, .. as rest] ->
            when command.subcommands is
                NoSubcommands -> Err KeyNotFound
                HasSubcommands scs ->
                    Dict.get scs first
                    |> Result.try \sc ->
                        findSubcommand sc rest

helpText : { config : CliConfig, subcommandPath ? List Str } -> Str
helpText = \{ config, subcommandPath ? [] } ->
    { config: command, path } = findSubcommandOrDefault config subcommandPath
    helpTextForCommand command path

helpTextForCommand : CliConfig, List Str -> Str
helpTextForCommand = \config, subcommandPath ->
    { version, authors, description, options, parameters, subcommands } = config

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

    $(usageHelp config subcommandPath)

    $([subcommandsText, parametersText, optionsText] |> Str.joinWith "\n\n")
    """

# TODO: consider showing required arguments in the usage
usageHelp : CliConfig, List Str -> Str
usageHelp = \config, path ->
    { config: { options, parameters, subcommands }, path: subcommandPath } = findSubcommandOrDefault config path
    name = Str.joinWith subcommandPath " "

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
