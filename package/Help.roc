interface Help
    exposes [helpText, helpTextForSubcommand]
    imports [Config.{ CliConfig, OptionConfig, SubcommandsConfig, ParameterConfig }]

# ArgExtractErr : [
#     MissingArg OptionConfig,
#     OptionCanOnlyBeSetOnce OptionConfig,
#     NoValueProvidedForOption OptionConfig,
#     OptionDoesNotExpectValue OptionConfig,
#     CannotUsePartialShortGroupAsValue OptionConfig (List Str),
#     InvalidNumArg OptionConfig,
#     InvalidCustomArg OptionConfig Str,
#     FailedToParseArgs ArgParseErr,
#     MissingParam ParameterConfig,
#     TooManyParamsProvided ParameterConfig,
#     UnrecognizedShortArg Str,
#     UnrecognizedLongArg Str,
# ]

# optionName : OptionConfig -> Str

# CliConfig : {
#     name : Str,
#     authors : List Str,
#     version : Str,
#     description : Str,
#     subcommands : Dict Str {
#         description : Str,
#         subcommands : SubcommandsConfig,
#         options : List OptionConfig,
#         parameters : List ParameterConfig,
#     },
#     options : List {
#         type : [Str, Num, Bool, Custom Str],
#         plurality : [Option, One, Many],
#         argsNeeded : [Zero, One],
#         short : Str,
#         long : Str,
#         name : Str,
#         help : Str,
#     },
#     parameters : List {
#         name : Str,
#         help : Str,
#         type : [Str, Num, Bool, Custom Str],
#         plurality : [Option, One, Many],
#     }
# }

helpTextForSubcommand : CliConfig, List Str -> Str
helpTextForSubcommand = \_config, _path ->
    "TODO"

helpText : CliConfig -> Str
helpText = \config ->
    authors =
        if List.isEmpty config.authors then
            "$(Str.joinWith config.authors " ")\n"
        else
            "\n"

    description =
        if config.description != "" then
            "$(config.description)\n"
        else
            "\n"

    """
    $(config.name) $(config.version)
    $(authors)
    $(description)
    $(usageText config)

    $(commandsText config.subcommands)

    $(paramsText config.parameters)

    $(optionsText config.options)
    """

usageText : CliConfig -> Str
usageText = \config ->
    options =
        if List.isEmpty config.options then
            ""
        else
            " [OPTIONS]"

    params =
        config.parameters
        |> List.map \param ->
            ellipsis =
                when param.plurality is
                    Optional | One -> ""
                    Many -> "..."

            " [$(param.name)]$(ellipsis)"
        |> Str.joinWith ""

    subcommandUsage =
        when config.subcommands is
            HasSubcommands sc if !(Dict.isEmpty sc) -> "  $(config.name) <COMMAND>\n"
            _other -> ""

    """
    Usage:
      $(config.name)$(options)$(params)
    $(subcommandUsage)
    """

commandsText : SubcommandsConfig -> Str
commandsText = \subcommands ->
    commands =
        when subcommands is
            NoSubcommands -> []
            HasSubcommands sc -> Dict.toList sc

    longestCommand =
        commands
        |> List.map \(name, _subConfig) -> List.len (Str.toUtf8 name)
        |> List.max
        |> Result.withDefault 0

    alignedCommands =
        commands
        |> List.map \(name, subConfig) ->
            nameLen = List.len (Str.toUtf8 name)
            buffer = Str.repeat " " (longestCommand - nameLen)

            "  $(name)$(buffer)  $(subConfig.description)"
        |> Str.joinWith "\n"

    """
    Commands:
    $(alignedCommands)
    """

paramsText : List ParameterConfig -> Str
paramsText = \params ->
    formattedParams =
        params
        |> List.map \param ->
            ellipsis =
                when param.plurality is
                    Optional | One -> ""
                    Many -> "..."

            ("[$(param.name)]$(ellipsis)", param.help)

    maxNameLen =
        formattedParams
        |> List.map \(name, _help) -> List.len (Str.toUtf8 name)
        |> List.max
        |> Result.withDefault 0

    alignedParams =
        formattedParams
        |> List.map \(name, help) ->
            buffer = Str.repeat " " (maxNameLen - List.len (Str.toUtf8 name))
            "  $(name)$(buffer)  $(help)"

    """
    Arguments:
    $(Str.joinWith alignedParams "\n")
    """

optionsText : List OptionConfig -> Str
optionsText = \options ->
    optionNameFormatter =
        if List.all options \o -> Str.isEmpty o.short then
            \opt -> "--$(opt.long)"
        else if List.all options \o -> Str.isEmpty o.long then
            \opt -> "-$(opt.short)"
        else
            \opt ->
                when (opt.short, opt.long) is
                    ("", "") -> ""
                    (short, "") -> "-$(short)"
                    ("", long) -> "    --$(long)"
                    (short, long) -> "-$(short), --$(long)"

    formattedOptions =
        List.map options \option ->
            (optionNameFormatter option, option.help)

    maxNameLen =
        formattedOptions
        |> List.map \(name, _help) -> List.len (Str.toUtf8 name)
        |> List.max
        |> Result.withDefault 0

    alignedOptions =
        List.map formattedOptions \(name, help) ->
            buffer = Str.repeat " " (maxNameLen - List.len (Str.toUtf8 name))
            "  $(name)$(buffer)  $(help)"

    """
    Options:
    $(Str.joinWith alignedOptions "\n")
    """

