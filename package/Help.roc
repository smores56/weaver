module [helpText, usageHelp]

import Base exposing [
    TextStyle,
    CliConfig,
    OptionConfig,
    ParameterConfig,
    SubcommandConfig,
    SubcommandsConfig,
]
import Utils exposing [toUpperCase, strLen]

# TODO: use roc-ansi once module params fix importing packages
bold_ansi_code = "\u(001b)[1m"
bold_and_underline_ansi_code = "\u(001b)[1m\u(001b)[4m"
reset_ansi_code = "\u(001b)[0m"

## Walks the subcommand tree from the root CLI config and either
## returns the subcommand's config as if it were the root command if a
## subcommand is found, or just the root command's config otherwise.
find_subcommand_or_default : CliConfig, List Str -> { config : CliConfig, subcommandPath : List Str }
find_subcommand_or_default = \config, subcommand_path ->
    base_command = {
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
find_subcommand : SubcommandConfig, List Str -> Result SubcommandConfig [KeyNotFound]
find_subcommand = \command, path ->
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
##     Opt.count { short: "v", help: "How verbose our logs should be." }
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
##           example -v [OPTIONS]
##
##         Options:
##           -v             How verbose our logs should be.
##           -h, --help     Show this help page.
##           -V, --version  Show the version.
##         """
## ```
help_text : CliConfig, List Str, TextStyle -> Str
help_text = \base_config, path, text_style ->
    { config, subcommand_path } = findSubcommandOrDefault baseConfig path
    { version, authors, description, options, parameters, subcommands } = config

    name = subcommandPath |> Str.joinWith " "

    top_line =
        [name, version]
        |> List.dropIf Str.isEmpty
        |> Str.joinWith " "

    authors_text =
        if List.isEmpty authors then
            ""
        else
            Str.concat "\n" (Str.joinWith authors " ")

    description_text =
        if Str.isEmpty description then
            ""
        else
            Str.concat "\n\n" description

    subcommands_text =
        when subcommands is
            HasSubcommands scs if !(Dict.isEmpty scs) ->
                commandsHelp subcommands textStyle

            _noSubcommands -> ""

    parameters_text =
        if List.isEmpty parameters then
            ""
        else
            parametersHelp parameters textStyle

    options_text =
        if List.isEmpty options then
            ""
        else
            optionsHelp options textStyle

    bottom_sections =
        [subcommandsText, parametersText, optionsText]
        |> List.dropIf Str.isEmpty
        |> Str.joinWith "\n\n"

    (style, reset) =
        when textStyle is
            Color -> (boldAndUnderlineAnsiCode, resetAnsiCode)
            Plain -> ("", "")

    """
    $(style)$(topLine)$(reset)$(authorsText)$(descriptionText)

    $(usageHelp config subcommandPath textStyle)

    $(bottomSections)
    """

## Render just the usage text for a command at or under the root config.
##
## The second argument should be a list of subcommand names, e.g.
## `["example", "subcommand-1", "subcommand-2"]`. If the subcommand
## isn't found, the root command's usage text is rendered by default.
##
## ```roc
## exampleCli =
##     Opt.count { short: "v", help: "How verbose our logs should be." }
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
##           example -v [OPTIONS]
##         """
## ```
usage_help : CliConfig, List Str, TextStyle -> Str
usage_help = \config, path, text_style ->
    { config: { options, parameters, subcommands }, subcommand_path } = findSubcommandOrDefault config path

    name = Str.joinWith subcommandPath " "

    required_options =
        options
        |> List.dropIf \opt -> opt.expectedValue == NothingExpected
        |> List.map optionSimpleNameFormatter

    other_options =
        if List.len requiredOptions == List.len options then
            []
        else
            ["[options]"]

    params_strings =
        parameters
        |> List.map \{ name: param_name, plurality } ->
            ellipsis =
                when plurality is
                    Optional | One -> ""
                    Many -> "..."

            "<$(paramName)$(ellipsis)>"

    first_line =
        requiredOptions
        |> List.concat otherOptions
        |> List.concat paramsStrings
        |> Str.joinWith " "

    subcommand_usage =
        when subcommands is
            HasSubcommands sc if !(Dict.isEmpty sc) -> "\n  $(name) <COMMAND>"
            _other -> ""

    (style, reset) =
        when textStyle is
            Color -> (boldAndUnderlineAnsiCode, resetAnsiCode)
            Plain -> ("", "")

    """
    $(style)Usage:$(reset)
      $(name) $(firstLine)$(subcommandUsage)
    """

commands_help : SubcommandsConfig, TextStyle -> Str
commands_help = \subcommands, text_style ->
    commands =
        when subcommands is
            NoSubcommands -> []
            HasSubcommands sc -> Dict.toList sc

    aligned_commands =
        commands
        |> List.map \(name, sub_config) ->
            (name, subConfig.description)
        |> alignTwoColumns textStyle

    (style, reset) =
        when textStyle is
            Color -> (boldAndUnderlineAnsiCode, resetAnsiCode)
            Plain -> ("", "")

    """
    $(style)Commands:$(reset)
    $(Str.joinWith alignedCommands "\n")
    """

parameters_help : List ParameterConfig, TextStyle -> Str
parameters_help = \params, text_style ->
    formatted_params =
        params
        |> List.map \param ->
            ellipsis =
                when param.plurality is
                    Optional | One -> ""
                    Many -> "..."

            ("<$(param.name)$(ellipsis)>", param.help)
        |> alignTwoColumns textStyle

    (style, reset) =
        when textStyle is
            Color -> (boldAndUnderlineAnsiCode, resetAnsiCode)
            Plain -> ("", "")

    """
    $(style)Arguments:$(reset)
    $(Str.joinWith formattedParams "\n")
    """

option_name_formatter : OptionConfig -> Str
option_name_formatter = \{ short, long, expected_value } ->
    short_name =
        if short != "" then
            "-$(short)"
        else
            ""

    long_name =
        if long != "" then
            "--$(long)"
        else
            ""

    type_name =
        when expectedValue is
            NothingExpected -> ""
            ExpectsValue name -> " $(toUpperCase name)"

    [shortName, longName]
    |> List.dropIf Str.isEmpty
    |> List.map \name -> Str.concat name typeName
    |> Str.joinWith ", "

option_simple_name_formatter : OptionConfig -> Str
option_simple_name_formatter = \{ short, long, expected_value } ->
    short_name =
        if short != "" then
            "-$(short)"
        else
            ""

    long_name =
        if long != "" then
            "--$(long)"
        else
            ""

    type_name =
        when expectedValue is
            NothingExpected -> ""
            ExpectsValue name -> " $(toUpperCase name)"

    [shortName, longName]
    |> List.dropIf Str.isEmpty
    |> Str.joinWith "/"
    |> Str.concat typeName

options_help : List OptionConfig, TextStyle -> Str
options_help = \options, text_style ->
    formatted_options =
        options
        |> List.map \option ->
            (optionNameFormatter option, option.help)
        |> alignTwoColumns textStyle

    (style, reset) =
        when textStyle is
            Color -> (boldAndUnderlineAnsiCode, resetAnsiCode)
            Plain -> ("", "")

    """
    $(style)Options:$(reset)
    $(Str.joinWith formattedOptions "\n")
    """

indent_multiline_string_by : Str, U64 -> Str
indent_multiline_string_by = \string, indent_amount ->
    indentation = Str.repeat " " indentAmount

    string
    |> Str.splitOn "\n"
    |> List.mapWithIndex \line, index ->
        if index == 0 then
            line
        else
            Str.concat indentation line
    |> Str.joinWith "\n"

align_two_columns : List (Str, Str), TextStyle -> List Str
align_two_columns = \columns, text_style ->
    max_first_column_len =
        columns
        |> List.map \(first, _second) -> strLen first
        |> List.max
        |> Result.withDefault 0

    (style, reset) =
        when textStyle is
            Color -> (boldAnsiCode, resetAnsiCode)
            Plain -> ("", "")

    List.map columns \(first, second) ->
        buffer =
            Str.repeat " " (maxFirstColumnLen - strLen first)
        second_shifted =
            indentMultilineStringBy second (maxFirstColumnLen + 4)

        "  $(style)$(first)$(buffer)$(reset)  $(secondShifted)"
