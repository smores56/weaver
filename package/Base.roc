module [
    ArgParserResult,
    ArgParserParams,
    ArgParserState,
    ArgParser,
    on_successful_arg_parse,
    map_successfully_parsed,
    ArgExtractErr,
    str_type_name,
    num_type_name,
    TextStyle,
    ExpectedValue,
    Plurality,
    SpecialFlags,
    InvalidValue,
    DefaultValue,
    ValueParser,
    OptionConfigBaseParams,
    DefaultableOptionConfigBaseParams,
    OptionConfigParams,
    DefaultableOptionConfigParams,
    OptionConfig,
    help_option,
    version_option,
    ParameterConfigBaseParams,
    DefaultableParameterConfigBaseParams,
    ParameterConfigParams,
    DefaultableParameterConfigParams,
    ParameterConfig,
    CliConfigParams,
    CliConfig,
    SubcommandConfigParams,
    SubcommandConfig,
    SubcommandsConfig,
]

import Arg exposing [Arg]
import Parser exposing [ParsedArg]

## The result of attempting to parse args into config data.
ArgParserResult a : [
    ShowHelp { subcommand_path : List Str },
    ShowVersion,
    IncorrectUsage ArgExtractErr { subcommand_path : List Str },
    SuccessfullyParsed a,
]

## The parameters that an [ArgParser] takes to extract data
## from args.
ArgParserParams : { args : List ParsedArg, subcommand_path : List Str }

## The intermediate state that an [ArgParser] passes between
## different parsing steps.
ArgParserState a : { data : a, remaining_args : List ParsedArg, subcommand_path : List Str }

## A function that takes command line arguments and a subcommand,
## and attempts to extract configuration data from said arguments.
ArgParser a : ArgParserParams -> ArgParserResult (ArgParserState a)

## A bind operation for [ArgParserState].
##
## If an [ArgParser] successfully parses some data, then said data
## is provided to a callback and the resulting [ArgParserResult] is
## passed along in the newly bound [ArgParser].
on_successful_arg_parse : ArgParser a, (ArgParserState a -> ArgParserResult (ArgParserState b)) -> ArgParser b
on_successful_arg_parse = |result, mapper|
    |input|
        when result(input) is
            ShowVersion -> ShowVersion
            ShowHelp({ subcommand_path }) -> ShowHelp({ subcommand_path })
            IncorrectUsage(arg_extract_err, { subcommand_path }) -> IncorrectUsage(arg_extract_err, { subcommand_path })
            SuccessfullyParsed({ data, remaining_args, subcommand_path }) ->
                mapper({ data, remaining_args, subcommand_path })

## Maps successfully parsed data that was parsed by an [ArgParser]
## by a user-defined operation.
map_successfully_parsed : ArgParserResult a, (a -> b) -> ArgParserResult b
map_successfully_parsed = |result, mapper|
    when result is
        ShowVersion -> ShowVersion
        ShowHelp({ subcommand_path }) -> ShowHelp({ subcommand_path })
        IncorrectUsage(arg_extract_err, { subcommand_path }) -> IncorrectUsage(arg_extract_err, { subcommand_path })
        SuccessfullyParsed(parsed) ->
            SuccessfullyParsed(mapper(parsed))

## Errors that can occur while extracting values from command line arguments.
ArgExtractErr : [
    NoSubcommandCalled,
    MissingOption OptionConfig,
    OptionCanOnlyBeSetOnce OptionConfig,
    NoValueProvidedForOption OptionConfig,
    OptionDoesNotExpectValue OptionConfig,
    CannotUsePartialShortGroupAsValue OptionConfig (List Str),
    InvalidOptionValue InvalidValue OptionConfig,
    InvalidParamValue InvalidValue ParameterConfig,
    MissingParam ParameterConfig,
    UnrecognizedShortArg Str,
    UnrecognizedLongArg Str,
    ExtraParamProvided Arg,
]

str_type_name = "str"
num_type_name = "num"

## Whether help text should have fancy styling.
TextStyle : [Color, Plain]

## The type of value that an option expects to parse.
ExpectedValue : [ExpectsValue Str, NothingExpected]

## How many values an option/parameter can take.
Plurality : [Optional, One, Many]

## The two built-in flags that we parse automatically.
SpecialFlags : { help : Bool, version : Bool }

InvalidValue : [InvalidNumStr, InvalidValue Str, InvalidUtf8]

DefaultValue a : [NoDefault, Value a, Generate ({} -> a)]

## A parser that extracts an argument value from a string.
ValueParser a : Arg -> Result a InvalidValue

OptionConfigBaseParams : {
    short ?? Str,
    long ?? Str,
    help ?? Str,
}

DefaultableOptionConfigBaseParams a : {
    short ?? Str,
    long ?? Str,
    help ?? Str,
    default ?? DefaultValue a,
}

## Default-value options for creating an option.
OptionConfigParams a : {
    short ?? Str,
    long ?? Str,
    help ?? Str,
    type : Str,
    parser : ValueParser a,
}

## Default-value options for creating an option.
DefaultableOptionConfigParams a : {
    short ?? Str,
    long ?? Str,
    help ?? Str,
    type : Str,
    parser : ValueParser a,
    default ?? DefaultValue a,
}

## Metadata for options in our CLI building system.
OptionConfig : {
    expected_value : ExpectedValue,
    plurality : Plurality,
    short : Str,
    long : Str,
    help : Str,
}

## Metadata for the `-h/--help` option that we parse automatically.
help_option : OptionConfig
help_option = {
    short: "h",
    long: "help",
    help: "Show this help page.",
    expected_value: NothingExpected,
    plurality: Optional,
}

## Metadata for the `-V/--version` option that we parse automatically.
version_option : OptionConfig
version_option = {
    short: "V",
    long: "version",
    help: "Show the version.",
    expected_value: NothingExpected,
    plurality: Optional,
}

ParameterConfigBaseParams : {
    name : Str,
    help ?? Str,
}

DefaultableParameterConfigBaseParams a : {
    name : Str,
    help ?? Str,
    default ?? DefaultValue a,
}

## Default-value options for creating an parameter.
ParameterConfigParams a : {
    name : Str,
    help ?? Str,
    type : Str,
    parser : ValueParser a,
}

## Default-value options for creating an parameter.
DefaultableParameterConfigParams a : {
    name : Str,
    help ?? Str,
    type : Str,
    parser : ValueParser a,
    default ?? DefaultValue a,
}

## Metadata for parameters in our CLI building system.
ParameterConfig : {
    name : Str,
    help : Str,
    type : Str,
    plurality : Plurality,
}

## Default-value options for bundling an CLI.
CliConfigParams : {
    name : Str,
    authors ?? List Str,
    version ?? Str,
    description ?? Str,
    text_style ?? TextStyle,
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
    description ?? Str,
}

## Metadata for a set of subcommands under a parent command.
##
## Since subcommands can have their own sub-subcommands,
## this type alias needs to be an enum with an empty variant
## to avoid infinite recursion.
SubcommandsConfig : [
    NoSubcommands,
    HasSubcommands (Dict Str {
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
