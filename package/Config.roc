interface Config
    exposes [
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
        SubcommandsConfig,
        getSubcommandNames,
    ]
    imports []

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
        (List {
            name : Str,
            description : Str,
            subcommands : SubcommandsConfig,
            options : List OptionConfig,
            parameters : List ParameterConfig,
        }),
]

getSubcommandNames : SubcommandsConfig -> List Str
getSubcommandNames = \config ->
    when config is
        NoSubcommands -> []
        HasSubcommands configs ->
            List.map configs .name
