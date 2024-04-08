interface ErrorFormatter
    exposes [formatArgExtractErr, formatCliValidationErr]
    imports [
        Config.{ ArgExtractErr, OptionName, OptionConfig },
        Validate.{ CliValidationErr },
    ]

optionDisplayName : { name : OptionName }* -> Str
optionDisplayName = \option ->
    when option.name is
        Short s -> "-$(s)"
        Long l -> "--$(l)"
        Both s l -> "-$(s)/--$(l)"

optionTypeName : OptionConfig -> Str
optionTypeName = \option ->
    when option.expectedType is
        None -> ""
        Str -> "string"
        Num -> "number"
        Custom c -> c

formatArgExtractErr : ArgExtractErr -> Str
formatArgExtractErr = \err ->
    when err is
        MissingOption option ->
            "Required option $(optionDisplayName option) is missing."

        OptionCanOnlyBeSetOnce option ->
            "Option $(optionDisplayName option) can only be set once."

        NoValueProvidedForOption option ->
            "Option $(optionDisplayName option) expects a $(optionTypeName option)."

        OptionDoesNotExpectValue option ->
            "Option $(optionDisplayName option) does not expect a value."

        CannotUsePartialShortGroupAsValue option partialGroup ->
            renderedGroup = "-$(Str.joinWith partialGroup "")"

            "The short option group $(renderedGroup) was partially consumed and cannot be used as a value for $(optionDisplayName option)."

        InvalidNumArg option ->
            "The value provided to $(optionDisplayName option) was not a valid number."

        InvalidCustomArg option reason ->
            "The value provided to $(optionDisplayName option) was not a valid $(optionTypeName option): $(reason)."

        MissingParam parameter ->
            "The parameter $(parameter |> .name) did not receive a value."

        UnrecognizedShortArg short ->
            "The argument -$(short) was not recognized."

        UnrecognizedLongArg long ->
            "The argument --$(long) was not recognized."

        ExtraParamProvided param ->
            "The parameter $(param) was not expected."

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
