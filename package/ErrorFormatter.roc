## Render errors we encounter in a human-readable format so that
## they are readable for developers and users on failure.
interface ErrorFormatter
    exposes [formatArgExtractErr, formatCliValidationErr]
    imports [
        Base.{ ArgExtractErr, OptionName, OptionConfig, MaybeExpectedType },
        Validate.{ CliValidationErr },
    ]

optionDisplayName : { short : Str, long : Str }* -> Str
optionDisplayName = \option ->
    when (option.short, option.long) is
        ("", "") -> ""
        (short, "") -> "-$(short)"
        ("", long) -> "--$(long)"
        (short, long) -> "-$(short)/--$(long)"

typeName : [Str, Num, Custom Str]* -> Str
typeName = \expectedType ->
    when expectedType is
        Str -> "string"
        Num -> "number"
        Custom c -> c
        _other -> ""

## Render [ArgExtractErr] errors as readable messages.
##
## Used in [Cli.parseOrDisplayMessage].
formatArgExtractErr : ArgExtractErr -> Str
formatArgExtractErr = \err ->
    when err is
        MissingOption option ->
            "Required option $(optionDisplayName option) is missing."

        OptionCanOnlyBeSetOnce option ->
            "Option $(optionDisplayName option) can only be set once."

        NoValueProvidedForOption option ->
            "Option $(optionDisplayName option) expects a $(option |> .expectedType |> typeName)."

        OptionDoesNotExpectValue option ->
            "Option $(optionDisplayName option) does not expect a value."

        CannotUsePartialShortGroupAsValue option partialGroup ->
            renderedGroup = "-$(Str.joinWith partialGroup "")"

            "The short option group $(renderedGroup) was partially consumed and cannot be used as a value for $(option |> .expectedType |> typeName)."

        InvalidNumArg option ->
            "The value provided to $(optionDisplayName option) was not a valid number."

        InvalidCustomArg option reason ->
            "The value provided to $(optionDisplayName option) was not a valid $(option |> .expectedType |> typeName): $(reason)."

        InvalidNumParam param ->
            "The value provided to the '$(param |> .name)' parameter was not a valid number."

        InvalidCustomParam param reason ->
            "The value provided to the '$(param |> .name)' parameter was not a valid $(param |> .type |> typeName): $(reason)."

        MissingParam parameter ->
            "The parameter $(parameter |> .name) did not receive a value."

        UnrecognizedShortArg short ->
            "The argument -$(short) was not recognized."

        UnrecognizedLongArg long ->
            "The argument --$(long) was not recognized."

        ExtraParamProvided param ->
            "The parameter $(param) was not expected."

## Render [CliValidationErr] errors as readable messages.
##
## Displayed as the crash message when [Cli.assertValid] fails.
formatCliValidationErr : CliValidationErr -> Str
formatCliValidationErr = \err ->
    valueAtSubcommandName = \{ name, subcommandPath } ->
        subcommandPathSuffix =
            if List.len subcommandPath <= 1 then
                ""
            else
                " for command '$(Str.joinWith subcommandPath " ")'"

        "$(name)$(subcommandPathSuffix)"

    optionAtSubcommandName = \{ option, subcommandPath } ->
        valueAtSubcommandName { name: "option '$(optionDisplayName option)'", subcommandPath }

    paramAtSubcommandName = \{ name, subcommandPath } ->
        valueAtSubcommandName { name: "parameter '$(name)'", subcommandPath }

    when err is
        OverlappingOptionNames option1 option2 ->
            "The $(optionAtSubcommandName option1) overlaps with the $(optionAtSubcommandName option2)."

        OverlappingParameterNames { first, second, subcommandPath } ->
            "The $(paramAtSubcommandName { name: first, subcommandPath }) overlaps with the $(paramAtSubcommandName { name: second, subcommandPath })."

        InvalidShortFlagName { name, subcommandPath } ->
            valueName = "option '-$(name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) is not a single character."

        InvalidLongFlagName { name, subcommandPath } ->
            valueName = "option '--$(name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) is not kebab-case and at least two characters."

        InvalidCommandName { name, subcommandPath } ->
            valueName = "command '$(name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) is not kebab-case."

        InvalidParameterName { name, subcommandPath } ->
            valueName = "parameter '$(name)'"
            "The $(valueAtSubcommandName { name: valueName, subcommandPath }) is not kebab-case."

        OptionMustHaveShortOrLongName { subcommandPath } ->
            "An $(valueAtSubcommandName { name: "option", subcommandPath }) has neither a short or long name."

        OverrodeSpecialHelpFlag { option, subcommandPath } ->
            "The $(optionAtSubcommandName { option, subcommandPath }) tried to overwrite the built-in -h/--help flag."

        OverrodeSpecialVersionFlag { option, subcommandPath } ->
            "The $(optionAtSubcommandName { option, subcommandPath }) tried to overwrite the built-in -V/--version flag."
