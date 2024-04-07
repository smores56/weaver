interface Validate
    exposes [validateCli, CliValidationErr]
    imports [
        Utils.{ strLen },
        Config.{
            OptionConfig,
            ParameterConfig,
            SubcommandsConfig,
            CliConfig,
        },
    ]

OptionAtSubcommand : { option : OptionConfig, subcommandPath : List Str }

CliValidationErr : [
    OverlappingOptionNames OptionAtSubcommand OptionAtSubcommand,
    InvalidShortFlagName { name : Str, subcommandPath : List Str },
    InvalidLongFlagName { name : Str, subcommandPath : List Str },
    InvalidCommandName { name : Str, subcommandPath : List Str },
    InvalidParameterName { name : Str, subcommandPath : List Str },
    OptionMustHaveShortOrLongFlag { option : OptionConfig, subcommandPath : List Str },
]

validateCli : CliConfig -> Result {} CliValidationErr
validateCli = \{ name, options, parameters, subcommands } ->
    validateCommand {
        name,
        options,
        parentOptions: [],
        parameters,
        subcommands,
        subcommandPath: [],
    }

validateCommand : { name : Str, options : List OptionConfig, parentOptions : List OptionAtSubcommand, parameters : List ParameterConfig, subcommands : SubcommandsConfig, subcommandPath : List Str } -> Result {} CliValidationErr
validateCommand = \{ name, options, parentOptions, parameters, subcommands, subcommandPath } ->
    {} <- ensureCommandIsWellNamed { name, subcommandPath }
        |> Result.try
    _ <- options
        |> List.mapTry \option -> ensureOptionIsWellNamed { option, subcommandPath }
        |> Result.try
    _ <- parameters
        |> List.mapTry \param -> ensureParamIsWellNamed { name: param.name, subcommandPath }
        |> Result.try

    when subcommands is
        HasSubcommands subcommandConfigs ->
            subcommandConfigs
            |> Dict.toList
            |> List.mapTry \(subcommandName, subcommand) ->
                updatedParentOptions =
                    options
                    |> List.map \option -> { option, subcommandPath }
                    |> List.concat parentOptions

                validateCommand {
                    name: subcommandName,
                    options: subcommand.options,
                    parentOptions: updatedParentOptions,
                    parameters: subcommand.parameters,
                    subcommands: subcommand.subcommands,
                    subcommandPath: subcommandPath |> List.append subcommandName,
                }
            |> Result.map \_successes -> {}

        NoSubcommands ->
            allOptionsToCheck =
                options
                |> List.map \option -> { option, subcommandPath }
                |> List.concat parentOptions

            checkIfThereAreOverlappingOptions allOptionsToCheck

charIsCamelCase : U8 -> Bool
charIsCamelCase = \char ->
    lowerAAsciiCode = 97
    lowerZAsciiCode = 122
    upperAAsciiCode = 65
    upperZAsciiCode = 90
    dashAsciiCode = 45

    (char >= lowerAAsciiCode && char <= lowerZAsciiCode)
    || (char >= upperAAsciiCode && char <= upperZAsciiCode)
    || (char == dashAsciiCode)

ensureCommandIsWellNamed : { name : Str, subcommandPath : List Str } -> Result {} CliValidationErr
ensureCommandIsWellNamed = \{ name, subcommandPath } ->
    nameIsCamelCase = Str.toUtf8 name |> List.all charIsCamelCase
    if name != "" && nameIsCamelCase then
        Ok {}
    else
        Err (InvalidCommandName { name, subcommandPath })

ensureParamIsWellNamed : { name : Str, subcommandPath : List Str } -> Result {} CliValidationErr
ensureParamIsWellNamed = \{ name, subcommandPath } ->
    nameIsCamelCase = Str.toUtf8 name |> List.all charIsCamelCase
    if strLen name >= 1 && nameIsCamelCase then
        Ok {}
    else
        Err (InvalidParameterName { name, subcommandPath })

ensureOptionIsWellNamed : { option : OptionConfig, subcommandPath : List Str } -> Result {} CliValidationErr
ensureOptionIsWellNamed = \{ option, subcommandPath } ->
    when (option.short, option.long) is
        ("", "") -> Err (OptionMustHaveShortOrLongFlag { option, subcommandPath })
        (short, "") -> ensureShortFlagIsWellNamed { name: short, subcommandPath }
        ("", long) -> ensureLongFlagIsWellNamed { name: long, subcommandPath }
        (short, long) ->
            ensureShortFlagIsWellNamed { name: short, subcommandPath }
            |> Result.try \{} -> ensureLongFlagIsWellNamed { name: long, subcommandPath }

ensureShortFlagIsWellNamed : { name : Str, subcommandPath : List Str } -> Result {} CliValidationErr
ensureShortFlagIsWellNamed = \{ name, subcommandPath } ->
    if strLen name != 1 then
        Err (InvalidShortFlagName { name, subcommandPath })
    else
        Ok {}

ensureLongFlagIsWellNamed : { name : Str, subcommandPath : List Str } -> Result {} CliValidationErr
ensureLongFlagIsWellNamed = \{ name, subcommandPath } ->
    nameIsCamelCase = Str.toUtf8 name |> List.all charIsCamelCase
    if strLen name > 1 && nameIsCamelCase then
        Ok {}
    else
        Err (InvalidLongFlagName { name, subcommandPath })

checkIfThereAreOverlappingOptions : List OptionAtSubcommand -> Result {} CliValidationErr
checkIfThereAreOverlappingOptions = \options ->
    List.range { start: At 1, end: At (List.len options) }
    |> List.map \offset -> List.map2 options (List.dropFirst options offset) Pair
    |> List.mapTry \pairs ->
        pairs
        |> List.mapTry \Pair left right ->
            if left.option.short == right.option.short || left.option.long == right.option.long then
                Err (OverlappingOptionNames left right)
            else
                Ok {}
    |> Result.map \_sucesses -> {}
