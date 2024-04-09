interface Base
    exposes [
        ArgParserResult,
        ArgParserParams,
        ArgParserState,
        ArgParser,
        onSuccessfulArgParse,
        mapSuccessfullyParsed,
        ArgExtractErr,
        ExpectedType,
        Plurality,
        SpecialFlags,
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
    ]
    imports [Parser.{ Arg, ArgParseErr }]

ArgParserResult a : [
    ShowHelp { subcommandPath : List Str },
    ShowVersion,
    IncorrectUsage ArgExtractErr { subcommandPath : List Str },
    SuccessfullyParsed a,
]

ArgParserParams : { args : List Arg, subcommandPath : List Str }
ArgParserState a : { data : a, remainingArgs : List Arg, subcommandPath : List Str }
ArgParser a : ArgParserParams -> ArgParserResult (ArgParserState a)

onSuccessfulArgParse : ArgParser a, (ArgParserState a -> ArgParserResult (ArgParserState b)) -> ArgParser b
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
    InvalidNumParam ParameterConfig,
    InvalidCustomParam ParameterConfig Str,
    MissingParam ParameterConfig,
    UnrecognizedShortArg Str,
    UnrecognizedLongArg Str,
    ExtraParamProvided Str,
]

ExpectedType : [Str, Num, Custom Str]
MaybeExpectedType : [None, Str, Num, Custom Str]

Plurality : [Optional, One, Many]

SpecialFlags : { help : Bool, version : Bool }

OptionConfigParams : {
    short ? Str,
    long ? Str,
    help ? Str,
}

OptionConfig : {
    expectedType : MaybeExpectedType,
    plurality : Plurality,
    short : Str,
    long : Str,
    help : Str,
}

helpOption : OptionConfig
helpOption = { short: "h", long: "help", help: "Show this help page.", expectedType: None, plurality: Optional }

versionOption : OptionConfig
versionOption = { short: "V", long: "version", help: "Show the version.", expectedType: None, plurality: Optional }

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

# Tag union required to avoid cyclic alias
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

# Done to avoid infinite type recursion
SubcommandConfig : {
    description : Str,
    subcommands : SubcommandsConfig,
    options : List OptionConfig,
    parameters : List ParameterConfig,
}
