module [help_text, usage_help]

import Base exposing [
    TextStyle,
    CliConfig,
    OptionConfig,
    ParameterConfig,
    SubcommandConfig,
    SubcommandsConfig,
]
import Utils exposing [to_upper_case]

# TODO: use roc-ansi once module params fix importing packages
bold_ansi_code = "\u(001b)[1m"
bold_and_underline_ansi_code = "\u(001b)[1m\u(001b)[4m"
reset_ansi_code = "\u(001b)[0m"

## Walks the subcommand tree from the root CLI config and either
## returns the subcommand's config as if it were the root command if a
## subcommand is found, or just the root command's config otherwise.
find_subcommand_or_default : CliConfig, List Str -> { config : CliConfig, subcommand_path : List Str }
find_subcommand_or_default = \config, subcommand_path ->
    base_command = {
        description: config.description,
        options: config.options,
        parameters: config.parameters,
        subcommands: config.subcommands,
    }

    when find_subcommand(base_command, List.drop_first(subcommand_path, 1)) is
        Err(KeyNotFound) -> { config, subcommand_path }
        Ok(c) ->
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
                subcommand_path,
            }

## Searches a command's config for subcommands recursively.
find_subcommand : SubcommandConfig, List Str -> Result SubcommandConfig [KeyNotFound]
find_subcommand = \command, path ->
    when path is
        [] -> Ok(command)
        [first, .. as rest] ->
            when command.subcommands is
                NoSubcommands -> Err(KeyNotFound)
                HasSubcommands(scs) ->
                    sc = try(Dict.get(scs, first))
                    find_subcommand(sc, rest)

## Render the help text for a command at or under the root config.
##
## The second argument should be a list of subcommand names, e.g.
## `["example", "subcommand-1", "subcommand-2"]`. If the subcommand
## isn't found, the root command's help page is rendered by default.
##
## ```roc
## example_cli =
##     Opt.count({ short: "v", help: "How verbose our logs should be." })
##     |> Cli.finish({
##         name: "example",
##         version: "v0.1.0",
##         description: "An example CLI.",
##     })
##     |> Cli.assert_valid
##
## expect
##     help_text(example_cli.config, ["example"])
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
    { config, subcommand_path } = find_subcommand_or_default(base_config, path)
    { version, authors, description, options, parameters, subcommands } = config

    name = subcommand_path |> Str.join_with(" ")

    top_line =
        [name, version]
        |> List.drop_if(Str.is_empty)
        |> Str.join_with(" ")

    authors_text =
        if List.is_empty(authors) then
            ""
        else
            Str.concat("\n", Str.join_with(authors, " "))

    description_text =
        if Str.is_empty(description) then
            ""
        else
            Str.concat("\n\n", description)

    subcommands_text =
        when subcommands is
            HasSubcommands(scs) if !(Dict.is_empty(scs)) ->
                commands_help(subcommands, text_style)

            _noSubcommands -> ""

    parameters_text =
        if List.is_empty(parameters) then
            ""
        else
            parameters_help(parameters, text_style)

    options_text =
        if List.is_empty(options) then
            ""
        else
            options_help(options, text_style)

    bottom_sections =
        [subcommands_text, parameters_text, options_text]
        |> List.drop_if(Str.is_empty)
        |> Str.join_with("\n\n")

    (style, reset) =
        when text_style is
            Color -> (bold_and_underline_ansi_code, reset_ansi_code)
            Plain -> ("", "")

    """
    $(style)$(top_line)$(reset)$(authors_text)$(description_text)

    $(usage_help(config, subcommand_path, text_style))

    $(bottom_sections)
    """

## Render just the usage text for a command at or under the root config.
##
## The second argument should be a list of subcommand names, e.g.
## `["example", "subcommand-1", "subcommand-2"]`. If the subcommand
## isn't found, the root command's usage text is rendered by default.
##
## ```roc
## example_cli =
##     Opt.count({ short: "v", help: "How verbose our logs should be." })
##     |> Cli.finish({
##         name: "example",
##         version: "v0.1.0",
##         description: "An example CLI.",
##     })
##     |> Cli.assert_valid
##
## expect
##     help_text(example_cli.config, ["example"])
##     ==
##         """
##         Usage:
##           example -v [OPTIONS]
##         """
## ```
usage_help : CliConfig, List Str, TextStyle -> Str
usage_help = \config, path, text_style ->
    { config: { options, parameters, subcommands }, subcommand_path } = find_subcommand_or_default(config, path)

    name = Str.join_with(subcommand_path, " ")

    required_options =
        options
        |> List.drop_if(\opt -> opt.expected_value == NothingExpected)
        |> List.map(option_simple_name_formatter)

    other_options =
        if List.len(required_options) == List.len(options) then
            []
        else
            ["[options]"]

    params_strings =
        parameters
        |> List.map(\{ name: param_name, plurality } ->
            ellipsis =
                when plurality is
                    Optional | One -> ""
                    Many -> "..."

            "<$(param_name)$(ellipsis)>")

    first_line =
        required_options
        |> List.concat(other_options)
        |> List.concat(params_strings)
        |> Str.join_with(" ")

    subcommand_usage =
        when subcommands is
            HasSubcommands(sc) if !(Dict.is_empty(sc)) -> "\n  $(name) <COMMAND>"
            _other -> ""

    (style, reset) =
        when text_style is
            Color -> (bold_and_underline_ansi_code, reset_ansi_code)
            Plain -> ("", "")

    """
    $(style)Usage:$(reset)
      $(name) $(first_line)$(subcommand_usage)
    """

commands_help : SubcommandsConfig, TextStyle -> Str
commands_help = \subcommands, text_style ->
    commands =
        when subcommands is
            NoSubcommands -> []
            HasSubcommands(sc) -> Dict.to_list(sc)

    aligned_commands =
        commands
        |> List.map(\(name, sub_config) ->
            (name, sub_config.description))
        |> align_two_columns(text_style)

    (style, reset) =
        when text_style is
            Color -> (bold_and_underline_ansi_code, reset_ansi_code)
            Plain -> ("", "")

    """
    $(style)Commands:$(reset)
    $(Str.join_with(aligned_commands, "\n"))
    """

parameters_help : List ParameterConfig, TextStyle -> Str
parameters_help = \params, text_style ->
    formatted_params =
        params
        |> List.map(\param ->
            ellipsis =
                when param.plurality is
                    Optional | One -> ""
                    Many -> "..."

            ("<$(param.name)$(ellipsis)>", param.help))
        |> align_two_columns(text_style)

    (style, reset) =
        when text_style is
            Color -> (bold_and_underline_ansi_code, reset_ansi_code)
            Plain -> ("", "")

    """
    $(style)Arguments:$(reset)
    $(Str.join_with(formatted_params, "\n"))
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
        when expected_value is
            NothingExpected -> ""
            ExpectsValue(name) -> " $(to_upper_case(name))"

    [short_name, long_name]
    |> List.drop_if(Str.is_empty)
    |> List.map(\name -> Str.concat(name, type_name))
    |> Str.join_with(", ")

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
        when expected_value is
            NothingExpected -> ""
            ExpectsValue(name) -> " $(to_upper_case(name))"

    [short_name, long_name]
    |> List.drop_if(Str.is_empty)
    |> Str.join_with("/")
    |> Str.concat(type_name)

options_help : List OptionConfig, TextStyle -> Str
options_help = \options, text_style ->
    formatted_options =
        options
        |> List.map(\option ->
            (option_name_formatter(option), option.help))
        |> align_two_columns(text_style)

    (style, reset) =
        when text_style is
            Color -> (bold_and_underline_ansi_code, reset_ansi_code)
            Plain -> ("", "")

    """
    $(style)Options:$(reset)
    $(Str.join_with(formatted_options, "\n"))
    """

indent_multiline_string_by : Str, U64 -> Str
indent_multiline_string_by = \string, indent_amount ->
    indentation = Str.repeat(" ", indent_amount)

    string
    |> Str.split_on("\n")
    |> List.map_with_index(\line, index ->
        if index == 0 then
            line
        else
            Str.concat(indentation, line))
    |> Str.join_with("\n")

align_two_columns : List (Str, Str), TextStyle -> List Str
align_two_columns = \columns, text_style ->
    max_first_column_len =
        columns
        |> List.map(\(first, _second) -> Str.count_utf8_bytes(first))
        |> List.max
        |> Result.with_default(0)

    (style, reset) =
        when text_style is
            Color -> (bold_ansi_code, reset_ansi_code)
            Plain -> ("", "")

    List.map(columns, \(first, second) ->
        buffer =
            Str.repeat(" ", (max_first_column_len - Str.count_utf8_bytes(first)))
        second_shifted =
            indent_multiline_string_by(second, (max_first_column_len + 4))

        "  $(style)$(first)$(buffer)$(reset)  $(second_shifted)")
