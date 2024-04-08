interface Config
    exposes [
        ArgParserResult,
        ArgParser,
        onSuccessfulArgParse,
        mapSuccessfullyParsed,
        ArgExtractErr,
        ExpectedType,
        Plurality,
        SpecialFlags,
        OptionName,
        optionShortName,
        optionLongName,
        OptionConfigParams,
        OptionConfig,
        helpOption,
        versionOption,
        ParameterConfigParams,
        ParameterConfig,
        CliConfigParams,
        CliConfig,
        SubcommandConfigParams,
        SubcommandConfig,
        SubcommandsConfig,
        getSubcommandNames,
    ]
    imports [Parser.{ Arg, ArgParseErr }]

ArgParserResult a : [
    ShowHelp { subcommandPath : List Str },
    ShowVersion,
    IncorrectUsage ArgExtractErr { subcommandPath : List Str },
    SuccessfullyParsed a,
]

ArgParser a : { args : List Arg, subcommandPath : List Str } -> ArgParserResult { data : a, remainingArgs : List Arg, subcommandPath : List Str }

onSuccessfulArgParse : ArgParser a, ({ data : a, remainingArgs : List Arg, subcommandPath : List Str } -> ArgParserResult { data : b, remainingArgs : List Arg, subcommandPath : List Str }) -> ArgParser b
onSuccessfulArgParse = \result, mapper ->
    \input ->
        when result input is
            ShowVersion -> ShowVersion
            ShowHelp { subcommandPath } -> ShowHelp { subcommandPath }
            IncorrectUsage argExtractErr { subcommandPath } -> IncorrectUsage argExtractErr { subcommandPath }
            SuccessfullyParsed { data, remainingArgs, subcommandPath } ->
                mapper { data, remainingArgs, subcommandPath }

mapSuccessfullyParsed : ArgParserResult a, (a -> b) -> ArgParserResult b
mapSuccessfullyParsed = \result, mapper ->
    when result is
        ShowVersion -> ShowVersion
        ShowHelp { subcommandPath } -> ShowHelp { subcommandPath }
        IncorrectUsage argExtractErr { subcommandPath } -> IncorrectUsage argExtractErr { subcommandPath }
        SuccessfullyParsed parsed ->
            SuccessfullyParsed (mapper parsed)

ArgExtractErr : [
    MissingOption OptionConfig,
    OptionCanOnlyBeSetOnce OptionConfig,
    NoValueProvidedForOption OptionConfig,
    OptionDoesNotExpectValue OptionConfig,
    CannotUsePartialShortGroupAsValue OptionConfig (List Str),
    InvalidNumArg OptionConfig,
    InvalidCustomArg OptionConfig Str,
    MissingParam ParameterConfig,
    UnrecognizedShortArg Str,
    UnrecognizedLongArg Str,
    ExtraParamProvided Str,
]

ExpectedType : [Str, Num, Custom Str]
MaybeExpectedType : [None, Str, Num, Custom Str]

Plurality : [Optional, One, Many]

OptionName : [Short Str, Long Str, Both Str Str]

optionShortName : OptionName -> Str
optionShortName = \name ->
    when name is
        Short short -> short
        Long _long -> ""
        Both short _long -> short

optionLongName : OptionName -> Str
optionLongName = \name ->
    when name is
        Short _short -> ""
        Long long -> long
        Both _short long -> long

SpecialFlags : { help : Bool, version : Bool }

OptionConfigParams : {
    name : OptionName,
    help ? Str,
}

OptionConfig : {
    expectedType : MaybeExpectedType,
    plurality : Plurality,
    name : OptionName,
    help : Str,
}

helpOption : OptionConfig
helpOption = { name: Both "h" "help", help: "Show this help page.", expectedType: None, plurality: Optional }

versionOption : OptionConfig
versionOption = { name: Both "V" "version", help: "Show the version.", expectedType: None, plurality: Optional }

ParameterConfigParams : {
    name : Str,
    help ? Str,
}

ParameterConfig : {
    name : Str,
    help : Str,
    type : ExpectedType,
    plurality : Plurality,
}

CliConfigParams : {
    name : Str,
    authors ? List Str,
    version ? Str,
    description ? Str,
}

CliConfig : {
    name : Str,
    authors : List Str,
    version : Str,
    description : Str,
    subcommands : SubcommandsConfig,
    options : List OptionConfig,
    parameters : List ParameterConfig,
}

SubcommandConfigParams : {
    name : Str,
    description ? Str,
}

# Tag union required to avoid infinite recursion
SubcommandsConfig : [
    NoSubcommands,
    HasSubcommands
        (Dict Str {
            description : Str,
            subcommands : SubcommandsConfig,
            options : List OptionConfig,
            parameters : List ParameterConfig,
        }),
]

# Done to avoid infinite recursion
SubcommandConfig : {
    description : Str,
    subcommands : SubcommandsConfig,
    options : List OptionConfig,
    parameters : List ParameterConfig,
}

getSubcommandNames : SubcommandsConfig -> List Str
getSubcommandNames = \config ->
    when config is
        NoSubcommands -> []
        HasSubcommands configs ->
            Dict.keys configs
