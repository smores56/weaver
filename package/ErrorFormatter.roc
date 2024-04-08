interface ErrorFormatter
    exposes [formatArgExtractErr, formatArgParseErr, formatCliValidationErr]
    imports [
        Config.{ ArgExtractErr, OptionName, OptionConfig },
        Parser.{ ArgParseErr },
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
        MissingArg option ->
            "Required option $(optionDisplayName option) is missing."

        OptionCanOnlyBeSetOnce option ->
            "Option $(optionDisplayName option) can only be set once."

        NoValueProvidedForOption option ->
            "Option $(optionDisplayName option) expects a $(optionTypeName option)."

        OptionDoesNotExpectValue option ->
            "Option $(optionDisplayName option) does not expect a value."

        CannotUsePartialShortGroupAsValue option partialGroup ->
            renderedGroup = "-$(Str.joinWith partialGroup "")"

            "The short option group $(renderedGroup) has been partially consumed and cannot be used as a value for $(optionDisplayName option)."

        InvalidNumArg option ->
            "The value provided to $(optionDisplayName option) is not a valid number."

        InvalidCustomArg option reason ->
            "The value provided to $(optionDisplayName option) is not a valid $(optionTypeName option): $(reason)."

        FailedToParseArgs argParseErr ->
            formatArgParseErr argParseErr

        MissingParam parameter ->
            "The parameter $(parameter |> .name) did not receive a value."

        TooManyParamsProvided parameter ->
            "The parameter $(parameter |> .name) received too many values."

        UnrecognizedShortArg short ->
            "The argument -$(short) was not recognized."

        UnrecognizedLongArg long ->
            "The argument --$(long) was not recognized."

formatArgParseErr : ArgParseErr -> Str
formatArgParseErr = \err ->
    when err is
        InvalidArg arg ->
            "The argument $(arg) is invalid."

formatCliValidationErr : CliValidationErr -> Str
formatCliValidationErr = \err ->
    valueAtSubcommandName = \{ name, subcommandPath } ->
        subcommandPathSuffix =
            if List.len subcommandPath <= 1 then
                ""
            else
                " for command $(Str.joinWith subcommandPath " ")"

        "$(name)$(subcommandPathSuffix)"

    optionAtSubcommandName = \{ option, subcommandPath } ->
        valueAtSubcommandName { name: "option '$(optionDisplayName option)'", subcommandPath }

    when err is
        OverlappingOptionNames option1 option2 ->
            "The $(optionAtSubcommandName option1) overlaps with the $(optionAtSubcommandName option2)."

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
