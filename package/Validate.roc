module [validate_cli, CliValidationErr]

import Utils exposing [is_kebab_case]
import Base exposing [
    OptionConfig,
    help_option,
    version_option,
    ParameterConfig,
    SubcommandsConfig,
    CliConfig,
]

OptionAtSubcommand : { option : OptionConfig, subcommand_path : List Str }

## The types of errors that might be found in a misconfigured CLI.
CliValidationErr : [
    OverlappingParameterNames { first : Str, second : Str, subcommand_path : List Str },
    OverlappingOptionNames OptionAtSubcommand OptionAtSubcommand,
    InvalidShortFlagName { name : Str, subcommand_path : List Str },
    InvalidLongFlagName { name : Str, subcommand_path : List Str },
    InvalidCommandName { name : Str, subcommand_path : List Str },
    InvalidParameterName { name : Str, subcommand_path : List Str },
    OptionMustHaveShortOrLongName { subcommand_path : List Str },
    InvalidOptionValueType { option : OptionConfig, subcommand_path : List Str },
    InvalidParameterValueType { param : ParameterConfig, subcommand_path : List Str },
    OverrodeSpecialHelpFlag { option : OptionConfig, subcommand_path : List Str },
    OverrodeSpecialVersionFlag { option : OptionConfig, subcommand_path : List Str },
]

## Ensure that a CLI's configuration is valid.
##
## Though the majority of the validation we'd need to do for type safety is
## rendered unnecessary by the design of this library, there are some things
## that the type system isn't able to prevent. Here are the checks we currently
## perform after building your CLI parser:
##
## - All commands and subcommands must have kebab-case names.
## - All options must have either:
##   - A short flag which is a single character.
##   - A long flag which is more than one character and kebab-case.
##   - Both a short and a long flag with the above requirements.
## - All parameters must be have kebab-case names.
## - No options can overlap, even between different subcommands, so long
##   as the options between the subcommands are ambiguous.
##   - For example, a CLI with a `-t` option at the root level and also
##     a `-t` option in the subcommand `sub` would fail validation since
##     we wouldn't know who should get the `-t` option.
##   - However, a CLI with two subcommands that each have a `-t` option
##     would not fail validation since only one subcommand can be called
##     at once.
validate_cli : CliConfig -> Result {} CliValidationErr
validate_cli = \{ name, options, parameters, subcommands } ->
    validate_command({
        name,
        options,
        parent_options: [],
        parameters,
        subcommands,
        subcommand_path: [name],
    })

validate_command :
    {
        name : Str,
        options : List OptionConfig,
        parent_options : List OptionAtSubcommand,
        parameters : List ParameterConfig,
        subcommands : SubcommandsConfig,
        subcommand_path : List Str,
    }
    -> Result {} CliValidationErr
validate_command = \{ name, options, parent_options, parameters, subcommands, subcommand_path } ->
    try(ensure_command_is_well_named({ name, subcommand_path }))

    _ = try(List.map_try(options, (\option ->
        try(ensure_option_is_well_named({ option, subcommand_path }))
        ensure_option_value_type_is_well_named({ option, subcommand_path }))))

    _ = try(List.map_try(parameters, (\param ->
        try(ensure_param_is_well_named({ name: param.name, subcommand_path }))
        ensure_param_value_type_is_well_named({ param, subcommand_path }))))

    try(check_if_there_are_overlapping_parameters(parameters, subcommand_path))

    when subcommands is
        HasSubcommands(subcommand_configs) if !(Dict.is_empty(subcommand_configs)) ->
            subcommand_configs
            |> Dict.to_list
            |> List.map_try(\(subcommand_name, subcommand) ->
                updated_parent_options =
                    options
                    |> List.map(\option -> { option, subcommand_path })
                    |> List.concat(parent_options)

                validate_command({
                    name: subcommand_name,
                    options: subcommand.options,
                    parent_options: updated_parent_options,
                    parameters: subcommand.parameters,
                    subcommands: subcommand.subcommands,
                    subcommand_path: subcommand_path |> List.append(subcommand_name),
                }))
            |> Result.map_ok(\_successes -> {})

        _no_subcommands ->
            all_options_to_check =
                options
                |> List.map(\option -> { option, subcommand_path })
                |> List.concat(parent_options)

            check_if_there_are_overlapping_options(all_options_to_check)

ensure_command_is_well_named : { name : Str, subcommand_path : List Str } -> Result {} CliValidationErr
ensure_command_is_well_named = \{ name, subcommand_path } ->
    if is_kebab_case(name) then
        Ok({})
    else
        Err(InvalidCommandName({ name, subcommand_path }))

ensure_param_is_well_named : { name : Str, subcommand_path : List Str } -> Result {} CliValidationErr
ensure_param_is_well_named = \{ name, subcommand_path } ->
    if is_kebab_case(name) then
        Ok({})
    else
        Err(InvalidParameterName({ name, subcommand_path }))

ensure_option_is_well_named : { option : OptionConfig, subcommand_path : List Str } -> Result {} CliValidationErr
ensure_option_is_well_named = \{ option, subcommand_path } ->
    when (option.short, option.long) is
        ("", "") -> Err(OptionMustHaveShortOrLongName({ subcommand_path }))
        (short, "") -> ensure_short_flag_is_well_named({ name: short, subcommand_path })
        ("", long) -> ensure_long_flag_is_well_named({ name: long, subcommand_path })
        (short, long) ->
            try(ensure_short_flag_is_well_named({ name: short, subcommand_path }))
            ensure_long_flag_is_well_named({ name: long, subcommand_path })

ensure_option_value_type_is_well_named : { option : OptionConfig, subcommand_path : List Str } -> Result {} CliValidationErr
ensure_option_value_type_is_well_named = \{ option, subcommand_path } ->
    when option.expected_value is
        ExpectsValue(type_name) ->
            if is_kebab_case(type_name) then
                Ok({})
            else
                Err(InvalidOptionValueType({ option, subcommand_path }))

        NothingExpected ->
            Ok({})

ensure_param_value_type_is_well_named : { param : ParameterConfig, subcommand_path : List Str } -> Result {} CliValidationErr
ensure_param_value_type_is_well_named = \{ param, subcommand_path } ->
    if is_kebab_case(param.type) then
        Ok({})
    else
        Err(InvalidParameterValueType({ param, subcommand_path }))

ensure_short_flag_is_well_named : { name : Str, subcommand_path : List Str } -> Result {} CliValidationErr
ensure_short_flag_is_well_named = \{ name, subcommand_path } ->
    if Str.count_utf8_bytes(name) != 1 then
        Err(InvalidShortFlagName({ name, subcommand_path }))
    else
        Ok({})

ensure_long_flag_is_well_named : { name : Str, subcommand_path : List Str } -> Result {} CliValidationErr
ensure_long_flag_is_well_named = \{ name, subcommand_path } ->
    if Str.count_utf8_bytes(name) > 1 && is_kebab_case(name) then
        Ok({})
    else
        Err(InvalidLongFlagName({ name, subcommand_path }))

ensure_option_names_do_not_overlap : OptionAtSubcommand, OptionAtSubcommand -> Result {} CliValidationErr
ensure_option_names_do_not_overlap = \left, right ->
    same_command = left.subcommand_path == right.subcommand_path
    either_name_matches =
        (left.option.short != "" && left.option.short == right.option.short)
        || (left.option.long != "" && left.option.long == right.option.long)

    matches_help =
        left.option.short == help_option.short || left.option.long == help_option.long
    matches_version =
        left.option.short == version_option.short || left.option.long == version_option.long

    if either_name_matches then
        if matches_help then
            if same_command then
                Err(OverrodeSpecialHelpFlag(left))
            else
                Ok({})
        else if matches_version then
            if same_command then
                Err(OverrodeSpecialVersionFlag(right))
            else
                Ok({})
        else
            Err(OverlappingOptionNames(left, right))
    else
        Ok({})

check_if_there_are_overlapping_options : List OptionAtSubcommand -> Result {} CliValidationErr
check_if_there_are_overlapping_options = \options ->
    List.range({ start: At(1), end: Before(List.len(options)) })
    |> List.map(\offset ->
        List.map2(options, List.drop_first(options, offset), Pair))
    |> List.map_try(\pairs ->
        pairs
        |> List.map_try(\Pair(left, right) ->
            ensure_option_names_do_not_overlap(left, right)))
    |> Result.map_ok(\_sucesses -> {})

check_if_there_are_overlapping_parameters : List ParameterConfig, List Str -> Result {} CliValidationErr
check_if_there_are_overlapping_parameters = \parameters, subcommand_path ->
    List.range({ start: At(1), end: Before(List.len(parameters)) })
    |> List.map(\offset ->
        List.map2(parameters, List.drop_first(parameters, offset), Pair))
    |> List.map_try(\pairs ->
        pairs
        |> List.map_try(\Pair(first, second) ->
            if first.name == second.name then
                Err(OverlappingParameterNames({ first: first.name, second: second.name, subcommand_path }))
            else
                Ok({})))
    |> Result.map_ok(\_sucesses -> {})
