interface Help
    exposes [helpText, helpTextForSubcommand]
    imports [Config.{ CliConfig }]

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

    $(commandsText config)
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

commandsText : CliConfig -> Str
commandsText = \config ->
    commands =
        when config.subcommands is
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

# optionsText : CliConfig -> Str
# optionsText = \config ->
#     options = config.options
#         |> List.map \opt ->

#             ()

#         """
#         Options:
#           $(alignedOptions)
#         """

# Roc makes two columns, spaced by the longest left column item plus two spaces
#
# command-name (version)
# authors
#
# description
#
# Usage:
#   command-name ...
#
# Commands:
#   abc          description description
#   def          description description
#   lorem-ipsum  description description
#
# Arguments:
#   [Arg_1]     description description
#   [Arg_2]...  description description
#
# Options:
#   -a, --auto      description description
#   -b, --beautify  description description
#   -c              description description
#       --delta     description description
#   -h, --help      Auto-add help text
#   -V, --version   Auto-add version
#
