interface Config
    exposes [
        DataParser,
        mapDataParserData,
        mapDataParserSubcommand,
        ArgExtractErr,
        ValueType,
        Plurality,
        ArgumentsNeeded,
        OptionConfigParams,
        OptionConfig,
        optionName,
        ParameterConfig,
        CliConfigParams,
        CliConfig,
        SubcommandConfigParams,
        SubcommandConfig,
        SubcommandsConfig,
        getSubcommandNames,
    ]
    imports [Parser.{ Arg, ArgParseErr }]

DataParser a s : List Arg -> Result ({ data : a, subcommand : Result s [NoSubcommand] }, List Arg) ArgExtractErr

mapDataParserData : DataParser a s, (a -> b) -> DataParser b s
mapDataParserData = \parser, mapper ->
    \args ->
        ({ data, subcommand }, remainingArgs) <- parser args
            |> Result.try

        Ok ({ data: mapper data, subcommand }, remainingArgs)

mapDataParserSubcommand : DataParser a s, (s -> t) -> DataParser a t
mapDataParserSubcommand = \parser, mapper ->
    \args ->
        ({ data, subcommand }, remainingArgs) <- parser args
            |> Result.try

        Ok ({ data, subcommand: subcommand |> Result.map mapper }, remainingArgs)

ArgExtractErr : [
    MissingArg OptionConfig,
    OptionCanOnlyBeSetOnce OptionConfig,
    NoValueProvidedForOption OptionConfig,
    OptionDoesNotExpectValue OptionConfig,
    # TODO: remove this by allowing what it prevents
    CannotUseGroupedShortArgAsValue OptionConfig Arg,
    InvalidNumArg OptionConfig,
    InvalidCustomArg OptionConfig Str,
    FailedToParseArgs ArgParseErr,
]

ValueType : [Str, Num, Bool, Custom Str]

Plurality : [Optional, One, Many]

ArgumentsNeeded : [Zero, One]

OptionConfigParams : {
    short ? Str,
    long ? Str,
    name ? Str,
    help ? Str,
}

OptionConfig : {
    type : ValueType,
    plurality : Plurality,
    argsNeeded : ArgumentsNeeded,
    short : Str,
    long : Str,
    name : Str,
    help : Str,
}

optionName : OptionConfig -> Str
optionName = \{ short, long, name } ->
    if name != "" then
        name
    else if long != "" then
        long
    else
        short

ParameterConfig : {
    name : Str,
    type : ValueType,
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

# mapSubcommandParser : SubcommandConfig s, (s -> t) -> SubcommandConfig t
# mapSubcommandParser = \config, mapper ->
#     childSubcommands =
#         when config.subcommands is
#             NoSubcommands -> NoSubcommands
#             HasSubcommands children ->
#                 children
#                 |> Dict.map \(name, child) -> (name, mapSubcommandParser child mapper)
#                 |> HasSubcommands

#     {
#         description: config.description,
#         subcommands: childSubcommands,
#         options: config.options,
#         parameters: config.parameters,
#         parser: mapDataParser config.parser mapper,
#     }
