interface Builder
    exposes [
        CliBuilder,
        cliBuilder,
        finishSubcommand,
        finishCli,
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

CliBuilder state s := {
    parser : DataParser state s,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : Dict Str { parser : DataParser s s, config : SubcommandConfig },
}

cliBuilder : List { name : Str, parser : DataParser s s, config : SubcommandConfig }, base -> CliBuilder base s
cliBuilder = \subcommandsConfig, base ->
    subcommands =
        subcommandsConfig
        |> List.map \{ name, parser, config } -> (name, { parser, config })
        |> Dict.fromList

    @CliBuilder {
        parser: \args -> Ok ({ data: base, subcommand: Err NoSubcommand }, args),
        options: [],
        parameters: [],
        subcommands,
    }

getValuesForOption :
    CliBuilder state s,
    OptionConfig,
    List Arg
    -> Result
        {
            data : state,
            subcommand : Result s [NoSubcommand],
            values : List ArgValue,
            remainingArgs : List Arg,
        }
        ArgExtractErr
getValuesForOption = \@CliBuilder builder, option, args ->
    ({ data, subcommand }, restOfArgs) <- builder.parser args
        |> Result.try

    when subcommand is
        Ok sc ->
            { values, remainingArgs, subcommandFound: _ } <- extractOptionValues { args: restOfArgs, option, subcommands: Dict.empty {} }
                |> Result.try

            Ok { data, subcommand: Ok sc, values, remainingArgs }

        Err NoSubcommand ->
            { values, remainingArgs, subcommandFound } <- extractOptionValues { args: restOfArgs, option, subcommands: builder.subcommands }
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

updateBuilder : CliBuilder (a -> state) s, DataParser state s, OptionConfig -> CliBuilder state s
updateBuilder = \@CliBuilder builder, parser, option ->
    @CliBuilder {
        parameters: builder.parameters,
        options: builder.options |> List.append option,
        subcommands: builder.subcommands,
        parser,
    }

finishSubcommand : CliBuilder state t, { name : Str, description : Str, mapper : state -> t } -> { name : Str, parser : DataParser t t, config : SubcommandConfig }
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
    CliBuilder state s,
    {
        name ? Str,
        authors ? List Str,
        version ? Str,
        description ? Str,
    }
    -> Result
        {
            config : CliConfig,
            parser : List Str -> Result { data : state, subcommand : Result s [NoSubcommand] } ArgExtractErr,
        }
        CliValidationErr
finishCli = \@CliBuilder builder, { name ? "", authors ? [], version ? "", description ? "" } ->
    parser = \args ->
        parsedArgs <- parseArgs args
            |> Result.mapErr FailedToParseArgs
            |> Result.try
        (state, _remainingArgs) <- builder.parser parsedArgs
            |> Result.try

        # TODO allow ensuring no unknown args were passed
        Ok state

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

numOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (I64 -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

strOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Str -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

customOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (a -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

maybeNumOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Result I64 [NoValue] -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

maybeStrOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Result Str [NoValue] -> state) s -> CliBuilder state s)
maybeStrOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: Optional, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getSingleValue values option
                |> Result.try

            Ok ({ data: data value, subcommand }, remainingArgs)

        updateBuilder builder newParser option

maybeCustomOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (Result a [NoValue] -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

numListOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (List I64 -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

strListOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (List Str -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

customListOption : { short ? Str, long ? Str, name ? Str, help ? Str, typeName : Str, parser : Str -> Result a [InvalidValue Str] } -> (CliBuilder (List a -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

flagOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Bool -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

occurrenceOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (U64 -> state) s -> CliBuilder state s)
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

        updateBuilder builder newParser option

unwrap : Result a err -> a where err implements Inspect
unwrap = \result ->
    when result is
        Ok val -> val
        Err err -> crash "$(Inspect.toStr err)"

expect
    { parser, config: _ } =
        cliBuilder [] {
            alpha: <- numOption { short: "a" },
            beta: <- flagOption { short: "b", long: "--beta" },
            xyz: <- strOption { long: "xyz" },
            verbosity: <- occurrenceOption { short: "v", long: "--verbose" },
        }
        |> finishCli { name: "app" }
        |> unwrap

    out = parser ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv"]

    out == Ok { data: { alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4 }, subcommand: Err NoSubcommand }

expect
    subcommandParser1 =
        cliBuilder [] {
            a: <- numOption { short: "a" },
            b: <- numOption { short: "b" },
        }
        |> finishSubcommand { name: "sp1", description: "", mapper: Sp1 }

    subcommandParser2 =
        cliBuilder [] {
            a: <- numOption { short: "a" },
            c: <- numOption { short: "c" },
        }
        |> finishSubcommand { name: "sp2", description: "", mapper: Sp2 }

    { parser, config: _ } =
        cliBuilder [subcommandParser1, subcommandParser2] {
            x: <- numOption { short: "x" },
        }
        |> finishCli { name: "app" }
        |> unwrap

    out = parser ["app", "-x", "123", "sp2", "-a", "456", "-c", "789"]

    out == Ok { data: { x: 123 }, subcommand: Ok (Sp2 { a: 456, c: 789 }) }

