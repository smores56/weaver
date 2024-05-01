module [helpText, usageHelp]

import Base exposing [
    CliConfig,
    OptionConfig,
    ParameterConfig,
    SubcommandConfig,
    SubcommandsConfig,
]
import Utils exposing [toUpperCase, strLen]

## Walks the subcommand tree from the root CLI config and either
## returns the subcommand's config as if it were the root command if a
## subcommand is found, or just the root command's config otherwise.
findSubcommandOrDefault : CliConfig, List Str -> { config : CliConfig, subcommandPath : List Str }
findSubcommandOrDefault = \config, subcommandPath ->
    baseCommand = {
        description: config.description,
        options: config.options,
        parameters: config.parameters,
        subcommands: config.subcommands,
    }

    when findSubcommand baseCommand (List.dropFirst subcommandPath 1) is
        Err KeyNotFound -> { config, subcommandPath }
        Ok c ->
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
                subcommandPath,
            }

## Searches a command's config for subcommands recursively.
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

## Render the help text for a command at or under the root config.
##
## The second argument should be a list of subcommand names, e.g.
## `["example", "subcommand-1", "subcommand-2"]`. If the subcommand
## isn't found, the root command's help page is rendered by default.
##
## ```roc
## exampleCli =
##     Cli.weave {
##         verbosity: <- Opt.count { short: "v", help: "How verbose our logs should be." },
##     }
##     |> Cli.finish {
##         name: "example",
##         version: "v0.1.0",
##         description: "An example CLI.",
##     }
##     |> Cli.assertValid
##
## expect
##     helpText exampleCli.config ["example"]
##     ==
##         """
##         example v0.1.0
##
##         An example CLI.
##
##         Usage:
##           example [OPTIONS]
##
##         Options:
##           -v             How verbose our logs should be.
##           -h, --help     Show this help page.
##           -V, --version  Show the version.
##         """
## ```
helpText : CliConfig, List Str -> Str
helpText = \baseConfig, path ->
    { config, subcommandPath } = findSubcommandOrDefault baseConfig path
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

    bottomSections =
        [subcommandsText, parametersText, optionsText]
        |> List.dropIf Str.isEmpty
        |> Str.joinWith "\n\n"

    """
    $(name) $(version)
    $(authorsText)
    $(descriptionText)

    $(usageHelp config subcommandPath)

    $(bottomSections)
    """

# TODO: consider showing required arguments in the usage
#
## Render just the usage text for a command at or under the root config.
##
## The second argument should be a list of subcommand names, e.g.
## `["example", "subcommand-1", "subcommand-2"]`. If the subcommand
## isn't found, the root command's usage text is rendered by default.
##
## ```roc
## exampleCli =
##     Cli.weave {
##         verbosity: <- Opt.count { short: "v", help: "How verbose our logs should be." },
##     }
##     |> Cli.finish {
##         name: "example",
##         version: "v0.1.0",
##         description: "An example CLI.",
##     }
##     |> Cli.assertValid
##
## expect
##     helpText exampleCli.config ["example"]
##     ==
##         """
##         Usage:
##           example [OPTIONS]
##         """
## ```
usageHelp : CliConfig, List Str -> Str
usageHelp = \config, path ->
    { config: { options, parameters, subcommands }, subcommandPath } = findSubcommandOrDefault config path

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
            HasSubcommands sc if !(Dict.isEmpty sc) -> "\n  $(name) <COMMAND>"
            _other -> ""

    """
    Usage:
      $(name)$(optionsStr)$(paramsStr)$(subcommandUsage)
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

optionNameFormatter : OptionConfig -> Str
optionNameFormatter = \{ short, long, expectedValue } ->
    shortName =
        if short != "" then
            "-$(short)"
        else
            ""

    longName =
        if long != "" then
            "--$(long)"
        else
            ""

    typeName =
        when expectedValue is
            NothingExpected -> ""
            ExpectsValue name -> " $(toUpperCase name)"

    [shortName, longName]
    |> List.dropIf Str.isEmpty
    |> List.map \name -> Str.concat name typeName
    |> Str.joinWith ", "

optionsHelp : List OptionConfig -> Str
optionsHelp = \options ->
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
