## Render errors we encounter in a human-readable format so that
## they are readable for developers and users on failure.
module [formatArgExtractErr, formatCliValidationErr]

import Base exposing [
    ArgExtractErr,
    ExpectedValue,
    strTypeName,
    numTypeName,
]
import Validate exposing [CliValidationErr]

option_display_name : { short : Str, long : Str }* -> Str
option_display_name = \option ->
    when (option.short, option.long) is
        ("", "") -> ""
        (short, "") -> "-$(short)"
        ("", long) -> "--$(long)"
        (short, long) -> "-$(short)/--$(long)"

option_type_name : { expectedValue : ExpectedValue }* -> Str
option_type_name = \{ expected_value } ->
    when expectedValue is
        ExpectsValue type_name -> fullTypeName typeName
        NothingExpected -> ""

full_type_name : Str -> Str
full_type_name = \type_name ->
    if typeName == strTypeName then
        "string"
    else if typeName == numTypeName then
        "number"
    else
        typeName

## Render [ArgExtractErr] errors as readable messages.
##
## Used in [Cli.parseOrDisplayMessage].
format_arg_extract_err : ArgExtractErr -> Str
format_arg_extract_err = \err ->
    when err is
        NoSubcommandCalled ->
            "A subcommand must be called."

        MissingOption option ->
            "Required option $(optionDisplayName option) is missing."

        OptionCanOnlyBeSetOnce option ->
            "Option $(optionDisplayName option) can only be set once."

        NoValueProvidedForOption option ->
            "Option $(optionDisplayName option) expects a $(optionTypeName option)."

        OptionDoesNotExpectValue option ->
            "Option $(optionDisplayName option) does not expect a value."

        CannotUsePartialShortGroupAsValue option partial_group ->
            rendered_group = "-$(Str.joinWith partialGroup "")"

            "The short option group $(renderedGroup) was partially consumed and cannot be used as a value for $(optionDisplayName option)."

        InvalidOptionValue value_err option ->
            when valueErr is
                InvalidNumStr ->
                    "The value provided to $(optionDisplayName option) was not a valid number."

                InvalidValue reason ->
                    "The value provided to $(optionDisplayName option) was not a valid $(optionTypeName option): $(reason)"

        InvalidParamValue value_err param ->
            when valueErr is
                InvalidNumStr ->
                    "The value provided to the '$(param |> .name)' parameter was not a valid number."

                InvalidValue reason ->
                    "The value provided to the '$(param |> .name)' parameter was not a valid $(param |> .type |> fullTypeName): $(reason)."

        MissingParam parameter ->
            "The '$(parameter |> .name)' parameter did not receive a value."

        UnrecognizedShortArg short ->
            "The argument -$(short) was not recognized."

        UnrecognizedLongArg long ->
            "The argument --$(long) was not recognized."

        ExtraParamProvided param ->
            "The parameter \"$(param)\" was not expected."

## Render [CliValidationErr] errors as readable messages.
##
## Displayed as the crash message when [Cli.assertValid] fails.
format_cli_validation_err : CliValidationErr -> Str
format_cli_validation_err = \err ->
    value_at_subcommand_name = \{ name, subcommand_path } ->
        subcommand_path_suffix =
            if List.len subcommandPath <= 1 then
                ""
            else
                " for command '$(Str.joinWith subcommandPath " ")'"

        "$(name)$(subcommandPathSuffix)"

    option_at_subcommand_name = \{ option, subcommand_path } ->
        valueAtSubcommandName { name: "option '$(optionDisplayName option)'", subcommandPath }

    param_at_subcommand_name = \{ name, subcommand_path } ->
        valueAtSubcommandName { name: "parameter '$(name)'", subcommandPath }

    when err is
        OverlappingOptionNames option1 option2 ->
            "The $(optionAtSubcommandName option1) overlaps with the $(optionAtSubcommandName option2)."

        OverlappingParameterNames { first, second, subcommand_path } ->
            "The $(paramAtSubcommandName { name: first, subcommandPath }) overlaps with the $(paramAtSubcommandName { name: second, subcommandPath })."

        InvalidShortFlagName { name, subcommand_path } ->
            value_name = "option '-$(name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) is not a single character."

        InvalidLongFlagName { name, subcommand_path } ->
            value_name = "option '--$(name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) is not kebab-case and at least two characters."

        InvalidCommandName { name, subcommand_path } ->
            value_name = "command '$(name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) is not kebab-case."

        InvalidParameterName { name, subcommand_path } ->
            value_name = "parameter '$(name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) is not kebab-case."

        OptionMustHaveShortOrLongName { subcommand_path } ->
            "An $(valueAtSubcommandName { name: "option", subcommandPath }) has neither a short or long name."

        InvalidOptionValueType { option, subcommand_path } ->
            value_type =
                when option.expectedValue is
                    ExpectsValue type_name -> typeName
                    NothingExpected -> ""

            "The $(optionAtSubcommandName { option, subcommandPath }) has value type '$(valueType)', which is not kebab-case."

        InvalidParameterValueType { param, subcommand_path } ->
            value_name = "parameter '$(param |> .name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) has value type '$(param |> .type)', which is not kebab-case."

        OverrodeSpecialHelpFlag { option, subcommand_path } ->
            "The $(optionAtSubcommandName { option, subcommandPath }) tried to overwrite the built-in -h/--help flag."

        OverrodeSpecialVersionFlag { option, subcommand_path } ->
            "The $(optionAtSubcommandName { option, subcommandPath }) tried to overwrite the built-in -V/--version flag."
