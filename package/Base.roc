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

## The result of attempting to parse args into config data.
ArgParserResult a : [
    ShowHelp { subcommandPath : List Str },
    ShowVersion,
    IncorrectUsage ArgExtractErr { subcommandPath : List Str },
    SuccessfullyParsed a,
]

## The parameters that an [ArgParser] takes to extract data
## from args.
ArgParserParams : { args : List Arg, subcommandPath : List Str }

## The intermediate state that an [ArgParser] passes between
## different parsing steps.
ArgParserState a : { data : a, remainingArgs : List Arg, subcommandPath : List Str }

## A function that takes command line arguments and a subcommand,
## and attempts to extract configuration data from said arguments.
ArgParser a : ArgParserParams -> ArgParserResult (ArgParserState a)

## A bind operation for [ArgParserState].
##
## If an [ArgParser] successfully parses some data, then said data
## is provided to a callback and the resulting [ArgParserResult] is
## passed along in the newly bound [ArgParser].
onSuccessfulArgParse : ArgParser a, (ArgParserState a -> ArgParserResult (ArgParserState b)) -> ArgParser b
onSuccessfulArgParse = \result, mapper ->
    \input ->
        when result input is
            ShowVersion -> ShowVersion
            ShowHelp { subcommandPath } -> ShowHelp { subcommandPath }
            IncorrectUsage argExtractErr { subcommandPath } -> IncorrectUsage argExtractErr { subcommandPath }
            SuccessfullyParsed { data, remainingArgs, subcommandPath } ->
                mapper { data, remainingArgs, subcommandPath }

## Maps successfully parsed data that was parsed by an [ArgParser]
## by a user-defined operation.
mapSuccessfullyParsed : ArgParserResult a, (a -> b) -> ArgParserResult b
mapSuccessfullyParsed = \result, mapper ->
    when result is
        ShowVersion -> ShowVersion
        ShowHelp { subcommandPath } -> ShowHelp { subcommandPath }
        IncorrectUsage argExtractErr { subcommandPath } -> IncorrectUsage argExtractErr { subcommandPath }
        SuccessfullyParsed parsed ->
            SuccessfullyParsed (mapper parsed)

## Errors that can occur while extracting values from command line arguments.
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

## The type of value that a parameter expects to parse.
ExpectedType : [Str, Num, Custom Str]

## The type of value that an option expects to parse.
MaybeExpectedType : [None, Str, Num, Custom Str]

## How many values an option/parameter can take.
Plurality : [Optional, One, Many]

## The two built-in flags that we parse automatically.
SpecialFlags : { help : Bool, version : Bool }

## Default-value options for creating an option.
OptionConfigParams : {
    short ? Str,
    long ? Str,
    help ? Str,
}

## Metadata for options in our CLI building system.
OptionConfig : {
    expectedType : MaybeExpectedType,
    plurality : Plurality,
    short : Str,
    long : Str,
    help : Str,
}

## Metadata for the `-h/--help` option that we parse automatically.
helpOption : OptionConfig
helpOption = {
    short: "h",
    long: "help",
    help: "Show this help page.",
    expectedType: None,
    plurality: Optional,
}

## Metadata for the `-V/--version` option that we parse automatically.
versionOption : OptionConfig
versionOption = {
    short: "V",
    long: "version",
    help: "Show the version.",
    expectedType: None,
    plurality: Optional,
}

## Default-value options for creating an parameter.
ParameterConfigParams : {
    name : Str,
    help ? Str,
}

## Metadata for parameters in our CLI building system.
ParameterConfig : {
    name : Str,
    help : Str,
    type : ExpectedType,
    plurality : Plurality,
}

## Default-value options for bundling an CLI.
CliConfigParams : {
    name : Str,
    authors ? List Str,
    version ? Str,
    description ? Str,
}

## Metadata for a root-level CLI.
CliConfig : {
    name : Str,
    authors : List Str,
    version : Str,
    description : Str,
    subcommands : SubcommandsConfig,
    options : List OptionConfig,
    parameters : List ParameterConfig,
}

## Default-value options for bundling a subcommand.
SubcommandConfigParams : {
    name : Str,
    description ? Str,
}

## Metadata for a set of subcommands under a parent command.
##
## Since subcommands can have their own sub-subcommands,
## this type alias needs to be an enum with an empty variant
## to avoid infinite recursion.
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

## Metadata for a subcommand.
SubcommandConfig : {
    description : Str,
    subcommands : SubcommandsConfig,
    options : List OptionConfig,
    parameters : List ParameterConfig,
}
