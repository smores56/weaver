interface Builder
    exposes [
        CliBuilder,
        cliBuilder,
        cliBuilderWithSubcommands,
        finishSubcommand,
        finishCli,
        subcommandField,
        strOption,
        numOption,
        customOption,
        maybeStrOption,
        maybeNumOption,
        maybeCustomOption,
        strListOption,
        numListOption,
        customListOption,
        flagOption,
        occurrenceOption,
    ]
    imports [
        Config.{
            DataParser,
            mapDataParserData,
            ArgExtractErr,
            OptionConfig,
            ParameterConfig,
            CliConfig,
            CliConfigParams,
            SubcommandConfig,
            SubcommandsConfig,
            SubcommandConfigParams,
        },
        Parser.{
            Arg,
            ArgValue,
            ParsedArgs,
            ArgParseErr,
            parseArgs,
        },
        Extract.{
            extractOptionValues,
            getSingleValue,
            getOptionalValue,
            parseNumValue,
            parseOptionalNumValue,
        },
        Validate.{ validateCli, CliValidationErr },
    ]

GetSubcommandsAction : { getSubcommands : {} }
GetOptionsAction : { getOptions : {} }
GetParamsAction : { getParams : {} }

CliBuilder state subState action := {
    parser : DataParser state s,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : Dict Str { parser : DataParser subState subState, config : SubcommandConfig },
}

cliBuilder : base -> CliBuilder base subState GetOptionsAction
cliBuilder = \base ->
    @CliBuilder {
        parser: \args -> Ok ({ data: base, subcommand: Err NoSubcommand }, args),
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

cliBuilderWithSubcommands : base -> CliBuilder base subState GetSubcommandsAction
cliBuilderWithSubcommands = \base ->
    @CliBuilder {
        parser: \args -> Ok ({ data: base, subcommand: Err NoSubcommand }, args),
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

getValuesForOption :
    CliBuilder state subState action,
    OptionConfig,
    List Arg
    -> Result
        {
            data : state,
            subcommand : Result subState [NoSubcommand],
            values : List ArgValue,
            remainingArgs : List Arg,
        }
        ArgExtractErr
getValuesForOption = \@CliBuilder builder, option, args ->
    ({ data, subcommand }, restOfArgs) <- builder.parser args
        |> Result.try

    when subcommand is
        Ok sc ->
            { values, remainingArgs, subcommandFound: _ } <- extractOptionValues {
                    args: restOfArgs,
                    option,
                    subcommands: Dict.empty {},
                }
                |> Result.try

            Ok { data, subcommand: Ok sc, values, remainingArgs }

        Err NoSubcommand ->
            { values, remainingArgs, subcommandFound } <- extractOptionValues {
                    args: restOfArgs,
                    option,
                    subcommands: builder.subcommands,
                }
                |> Result.try

            # TODO: what about child data?
            when subcommandFound is
                Ok found ->
                    # TODO: probably have to drop the first argument here, since it represents the name of the subcommand
                    ({ data: subcommandData, subcommand: _ }, otherArgs) <- found.parser found.args
                        |> Result.try

                    Ok { data, subcommand: Ok subcommandData, values, remainingArgs: otherArgs }

                Err NoSubcommand ->
                    Ok { data, subcommand: Err NoSubcommand, values, remainingArgs }

# getValuesForParam :
#     CliBuilder state subState action,
#     ParameterConfig,
#     List Arg
#     -> Result
#         {
#             data : state,
#             subcommand : Result subState [NoSubcommand],
#             values : List Str,
#             remainingArgs : List Arg,
#         }
#         ArgExtractErr
# getValuesForParam = \@CliBuilder builder, param, args ->
#     ({ data, subcommand }, restOfArgs) <- builder.parser args
#         |> Result.try

#     when subcommand is
#         Ok sc ->
#             { values, remainingArgs, subcommandFound: _ } <- extractOptionValues {
#                     args: restOfArgs,
#                     option,
#                     subcommands: Dict.empty {},
#                 }
#                 |> Result.try

#             Ok { data, subcommand: Ok sc, values, remainingArgs }

#         Err NoSubcommand ->
#             { values, remainingArgs, subcommandFound } <- extractOptionValues {
#                     args: restOfArgs,
#                     option,
#                     subcommands: builder.subcommands,
#                 }
#                 |> Result.try

#             # TODO: what about child data?
#             when subcommandFound is
#                 Ok found ->
#                     # TODO: probably have to drop the first argument here, since it represents the name of the subcommand
#                     ({ data: subcommandData, subcommand: _ }, otherArgs) <- found.parser found.args
#                         |> Result.try

#                     Ok { data, subcommand: Ok subcommandData, values, remainingArgs: otherArgs }

#                 Err NoSubcommand ->
#                     Ok { data, subcommand: Err NoSubcommand, values, remainingArgs }

updateBuilderWithOption : CliBuilder (a -> state) subState action, DataParser state s, OptionConfig -> CliBuilder state subState action
updateBuilderWithOption = \@CliBuilder builder, parser, option ->
    @CliBuilder {
        parameters: builder.parameters,
        options: builder.options |> List.append option,
        subcommands: builder.subcommands,
        parser,
    }

updateBuilderWithParam : CliBuilder (a -> state) subState action, DataParser state s, ParameterConfig -> CliBuilder state subState action
updateBuilderWithParam = \@CliBuilder builder, parser, param ->
    @CliBuilder {
        parameters: builder.parameters |> List.append param,
        options: builder.options,
        subcommands: builder.subcommands,
        parser,
    }

finishSubcommand : CliBuilder state t action, { name : Str, description : Str, mapper : state -> t } -> { name : Str, parser : DataParser t t, config : SubcommandConfig }
finishSubcommand = \@CliBuilder builder, { name, description, mapper } ->
    subcommands =
        builder.subcommands
        |> Dict.map \_n, sc -> sc.config
        |> HasSubcommands

    {
        name,
        config: {
            description,
            options: builder.options,
            parameters: builder.parameters,
            subcommands,
        },
        parser: mapDataParserData builder.parser mapper,
    }

finishCli :
    CliBuilder state subState action,
    {
        name ? Str,
        authors ? List Str,
        version ? Str,
        description ? Str,
    }
    -> Result
        {
            config : CliConfig,
            parser : List Str -> Result state ArgExtractErr,
        }
        CliValidationErr
finishCli = \@CliBuilder builder, { name ? "", authors ? [], version ? "", description ? "" } ->
    parser = \args ->
        parsedArgs <- parseArgs args
            |> Result.mapErr FailedToParseArgs
            |> Result.try
        ({ data, subcommand: _ }, _remainingArgs) <- builder.parser parsedArgs
            |> Result.try

        # TODO allow ensuring no unknown args were passed
        Ok data

    subcommands =
        builder.subcommands
        |> Dict.map \_n, sc -> sc.config
        |> HasSubcommands

    config = {
        name,
        authors,
        version,
        description,
        subcommands,
        options: builder.options,
        parameters: builder.parameters,
    }

    validateCli config
    |> Result.map \_ -> { config, parser }

subcommandField : List { name : Str, parser : DataParser subState subState, config : SubcommandConfig } -> (CliBuilder (Result subState [NoSubcommand] -> state) subState GetSubcommandsAction -> CliBuilder state subState GetOptionsAction)
subcommandField = \subcommandConfigs ->
    subcommands =
        subcommandConfigs
        |> List.map \{ name, parser, config } -> (name, { parser, config })
        |> Dict.fromList

    \@CliBuilder builder ->
        # TODO: rewrite this to make sure it works?
        newParser = \args ->
            when builder.parser args is
                Ok ({ data, subcommand }, remainingArgs) ->
                    Ok ({ data: data subcommand, subcommand }, remainingArgs)

                Err err -> Err err

        @CliBuilder {
            options: builder.options,
            parameters: builder.parameters,
            parser: newParser,
            subcommands,
        }

numOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (I64 -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
numOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: One, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            numValue <- getSingleValue values option
                |> Result.try \val -> parseNumValue val option
                |> Result.try

            Ok ({ data: data numValue, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

strOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Str -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
strOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: One, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getSingleValue values option
                |> Result.try

            when value is
                Ok val -> Ok ({ data: data val, subcommand }, remainingArgs)
                Err NoValue -> Err (NoValueProvidedForOption option)

        updateBuilderWithOption builder newParser option

customOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (a -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
customOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: One, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getSingleValue values option
                |> Result.try

            when value is
                Ok val ->
                    when parser val is
                        Ok parsedValue -> Ok ({ data: data parsedValue, subcommand }, remainingArgs)
                        Err (InvalidValue reason) -> Err (InvalidCustomArg option reason)

                Err NoValue -> Err (NoValueProvidedForOption option)

        updateBuilderWithOption builder newParser option

maybeNumOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Result I64 [NoValue] -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
maybeNumOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: Optional, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            numValue <- getSingleValue values option
                |> Result.try \val -> parseOptionalNumValue val option
                |> Result.try

            Ok ({ data: data numValue, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

maybeStrOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Result Str [NoValue] -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
maybeStrOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: Optional, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getSingleValue values option
                |> Result.try

            Ok ({ data: data value, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

maybeCustomOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (Result a [NoValue] -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
maybeCustomOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: Optional, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getSingleValue values option
                |> Result.try

            when value is
                Ok val ->
                    when parser val is
                        Ok parsedValue -> Ok ({ data: data (Ok parsedValue), subcommand }, remainingArgs)
                        Err (InvalidValue reason) -> Err (InvalidCustomArg option reason)

                Err NoValue -> Ok ({ data: data (Err NoValue), subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

numListOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (List I64 -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
numListOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: Many, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            numValues <- values
                |> List.mapTry \val -> parseNumValue val option
                |> Result.try

            Ok ({ data: data numValues, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

strListOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (List Str -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
strListOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: Many, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            strValues <- values
                |> List.mapTry \val -> val
                |> Result.mapErr \NoValue -> NoValueProvidedForOption option
                |> Result.try

            Ok ({ data: data strValues, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

customListOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (List a -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
customListOption = \{ short ? "", long ? "", name ? "", help ? "", typeName, parser } ->
    option = { type: Custom typeName, plurality: Many, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            strValues <- values
                |> List.mapTry \val -> val
                |> Result.mapErr \NoValue -> NoValueProvidedForOption option
                |> Result.try
            parsedValues <- strValues
                |> List.mapTry parser
                |> Result.mapErr \InvalidValue reason -> InvalidCustomArg option reason
                |> Result.try

            Ok ({ data: data parsedValues, subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

flagOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Bool -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
flagOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Bool, plurality: Optional, argsNeeded: Zero, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getOptionalValue values option
                |> Result.try

            when value is
                Err NoValue -> Ok ({ data: data Bool.false, subcommand }, remainingArgs)
                Ok (Err NoValue) -> Ok ({ data: data Bool.true, subcommand }, remainingArgs)
                Ok (Ok _val) -> Err (OptionDoesNotExpectValue option)

        updateBuilderWithOption builder newParser option

occurrenceOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (U64 -> state) subState GetOptionsAction -> CliBuilder state subState GetOptionsAction)
occurrenceOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Bool, plurality: Many, argsNeeded: Zero, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            if values |> List.any Result.isOk then
                Err (OptionDoesNotExpectValue option)
            else
                Ok ({ data: data (List.len values), subcommand }, remainingArgs)

        updateBuilderWithOption builder newParser option

# TODO: prep paramsBuilder
paramsBuilder : {}

strParam : { name : Str, help ? Str } -> (CliBuilder (List Str -> state) s GetOptionsAction -> CliBuilder state s GetParamsAction)
strParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: One }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForParam builder param args
                |> Result.try

            when values is
                [] -> Err (MissingParam param)
                [single] -> Ok ({ data: data single, subcommand }, remainingArgs)
                [..] -> Err (TooManyParamsProvided param)

        updateBuilderWithParam builder newParser param

# unwrap : Result a err -> a where err implements Inspect
# unwrap = \result ->
#     when result is
#         Ok val -> val
#         Err err -> crash "$(Inspect.toStr err)"

expect
    { parser, config: _ } =
        cliBuilder {
            alpha: <- numOption { short: "a" },
            beta: <- flagOption { short: "b", long: "--beta" },
            xyz: <- strOption { long: "xyz" },
            verbosity: <- occurrenceOption { short: "v", long: "--verbose" },
        }
        |> finishCli { name: "app" }
        |> unwrap

    out = parser ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv"]

    out == Ok { alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4 }

expect
    subSubcommandParser1 =
        cliBuilder {
            a: <- numOption { short: "a" },
            b: <- numOption { short: "b" },
        }
        |> finishSubcommand { name: "ss1", description: "", mapper: SS1 }

    subSubcommandParser2 =
        cliBuilder {
            a: <- numOption { short: "a" },
            c: <- numOption { short: "c" },
        }
        |> finishSubcommand { name: "ss2", description: "", mapper: SS2 }

    subcommandParser1 =
        cliBuilderWithSubcommands {
            sc: <- subcommandField [subSubcommandParser1, subSubcommandParser2],
            d: <- numOption { short: "d" },
            e: <- numOption { short: "e" },
        }
        |> finishSubcommand { name: "s1", description: "", mapper: S1 }

    subcommandParser2 =
        cliBuilder {
            d: <- numOption { short: "d" },
            f: <- numOption { short: "f" },
        }
        |> finishSubcommand { name: "s2", description: "", mapper: S2 }

    { parser, config: _ } =
        cliBuilderWithSubcommands {
            sc: <- subcommandField [subcommandParser1, subcommandParser2],
            x: <- numOption { short: "x" },
        }
        |> finishCli { name: "app" }
        |> unwrap

    out = parser ["app", "-x", "123", "s1", "-d", "456", "-e", "789", "ss2", "-a", "135", "-c", "246"]

    out == Ok { x: 123, sc: Ok (S1 { sc: Ok (SS2 { a: 135, c: 246 }), d: 456, e: 789 }) }

