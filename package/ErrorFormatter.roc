## Render errors we encounter in a human-readable format so that
## they are readable for developers and users on failure.
module [format_arg_extract_err, format_cli_validation_err]

import Arg
import Base exposing [
    ArgExtractErr,
    ExpectedValue,
    str_type_name,
    num_type_name,
]
import Validate exposing [CliValidationErr]

option_display_name : { short : Str, long : Str }* -> Str
option_display_name = \option ->
    when (option.short, option.long) is
        ("", "") -> ""
        (short, "") -> "-$(short)"
        ("", long) -> "--$(long)"
        (short, long) -> "-$(short)/--$(long)"

option_type_name : { expected_value : ExpectedValue }* -> Str
option_type_name = \{ expected_value } ->
    when expected_value is
        ExpectsValue(type_name) -> full_type_name(type_name)
        NothingExpected -> ""

full_type_name : Str -> Str
full_type_name = \type_name ->
    if type_name == str_type_name then
        "string"
    else if type_name == num_type_name then
        "number"
    else
        type_name

## Render [ArgExtractErr] errors as readable messages.
##
## Used in [Cli.parse_or_display_message].
format_arg_extract_err : ArgExtractErr -> Str
format_arg_extract_err = \err ->
    when err is
        NoSubcommandCalled ->
            "A subcommand must be called."

        MissingOption(option) ->
            "Required option $(option_display_name(option)) is missing."

        OptionCanOnlyBeSetOnce(option) ->
            "Option $(option_display_name(option)) can only be set once."

        NoValueProvidedForOption(option) ->
            "Option $(option_display_name(option)) expects a $(option_type_name(option))."

        OptionDoesNotExpectValue(option) ->
            "Option $(option_display_name(option)) does not expect a value."

        CannotUsePartialShortGroupAsValue(option, partial_group) ->
            rendered_group = "-$(Str.join_with(partial_group, ""))"

            "The short option group $(rendered_group) was partially consumed and cannot be used as a value for $(option_display_name(option))."

        InvalidOptionValue(value_err, option) ->
            when value_err is
                InvalidNumStr ->
                    "The value provided to $(option_display_name(option)) was not a valid number."

                InvalidValue(reason) ->
                    "The value provided to $(option_display_name(option)) was not a valid $(option_type_name(option)): $(reason)"

                InvalidUtf8 ->
                    "The value provided to $(option_display_name(option)) was not valid UTF-8."

        InvalidParamValue(value_err, param) ->
            when value_err is
                InvalidNumStr ->
                    "The value provided to the '$(param |> .name)' parameter was not a valid number."

                InvalidValue(reason) ->
                    "The value provided to the '$(param |> .name)' parameter was not a valid $(param |> .type |> full_type_name): $(reason)."

                InvalidUtf8 ->
                    "The value provided to the '$(param |> .name)' parameter was not valid UTF-8."

        MissingParam(parameter) ->
            "The '$(parameter |> .name)' parameter did not receive a value."

        UnrecognizedShortArg(short) ->
            "The argument -$(short) was not recognized."

        UnrecognizedLongArg(long) ->
            "The argument --$(long) was not recognized."

        ExtraParamProvided(param) ->
            "The parameter \"$(Arg.display(param))\" was not expected."

## Render [CliValidationErr] errors as readable messages.
##
## Displayed as the crash message when [Cli.assert_valid] fails.
format_cli_validation_err : CliValidationErr -> Str
format_cli_validation_err = \err ->
    value_at_subcommand_name = \{ name, subcommand_path } ->
        subcommand_path_suffix =
            if List.len(subcommand_path) <= 1 then
                ""
            else
                " for command '$(Str.join_with(subcommand_path, " "))'"

        "$(name)$(subcommand_path_suffix)"

    option_at_subcommand_name = \{ option, subcommand_path } ->
        value_at_subcommand_name({ name: "option '$(option_display_name(option))'", subcommand_path })

    param_at_subcommand_name = \{ name, subcommand_path } ->
        value_at_subcommand_name({ name: "parameter '$(name)'", subcommand_path })

    when err is
        OverlappingOptionNames(option1, option2) ->
            "The $(option_at_subcommand_name(option1)) overlaps with the $(option_at_subcommand_name(option2))."

        OverlappingParameterNames({ first, second, subcommand_path }) ->
            "The $(param_at_subcommand_name({ name: first, subcommand_path })) overlaps with the $(param_at_subcommand_name({ name: second, subcommand_path }))."

        InvalidShortFlagName({ name, subcommand_path }) ->
            value_name = "option '-$(name)'"
            "The $(value_at_subcommand_name({ name: value_name, subcommand_path })) is not a single character."

        InvalidLongFlagName({ name, subcommand_path }) ->
            value_name = "option '--$(name)'"
            "The $(value_at_subcommand_name({ name: value_name, subcommand_path })) is not kebab-case and at least two characters."

        InvalidCommandName({ name, subcommand_path }) ->
            value_name = "command '$(name)'"
            "The $(value_at_subcommand_name({ name: value_name, subcommand_path })) is not kebab-case."

        InvalidParameterName({ name, subcommand_path }) ->
            value_name = "parameter '$(name)'"
            "The $(value_at_subcommand_name({ name: value_name, subcommand_path })) is not kebab-case."

        OptionMustHaveShortOrLongName({ subcommand_path }) ->
            "An $(value_at_subcommand_name({ name: "option", subcommand_path })) has neither a short or long name."

        InvalidOptionValueType({ option, subcommand_path }) ->
            value_type =
                when option.expected_value is
                    ExpectsValue(type_name) -> type_name
                    NothingExpected -> ""

            "The $(option_at_subcommand_name({ option, subcommand_path })) has value type '$(value_type)', which is not kebab-case."

        InvalidParameterValueType({ param, subcommand_path }) ->
            value_name = "parameter '$(param |> .name)'"
            "The $(value_at_subcommand_name({ name: value_name, subcommand_path })) has value type '$(param |> .type)', which is not kebab-case."

        OverrodeSpecialHelpFlag({ option, subcommand_path }) ->
            "The $(option_at_subcommand_name({ option, subcommand_path })) tried to overwrite the built-in -h/--help flag."

        OverrodeSpecialVersionFlag({ option, subcommand_path }) ->
            "The $(option_at_subcommand_name({ option, subcommand_path })) tried to overwrite the built-in -V/--version flag."
