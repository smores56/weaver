interface Builder
    exposes [
        strOption,
        numOption,
        flagOption,
        occurrenceOption,
        cliBuilder,
        getParser,
    ]
    imports [
        Config.{ OptionConfig, ParameterConfig, SubcommandsConfig, getSubcommandNames },
        Parser.{ Arg, ArgValue, ParsedArgs, ArgParseErr, parseArgs },
        Extract.{
            extractOptionValues,
            getSingleValue,
            getOptionalValue,
            parseNumValue,
            ArgExtractErr,
        },
    ]

ValueParser out : List Arg -> Result (out, List Arg) ArgExtractErr

CliBuilder state s := {
    parser : ValueParser state,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : SubcommandsConfig s,
}

cliBuilder : base -> CliBuilder base s
cliBuilder = \base ->
    @CliBuilder {
        parser: \args -> Ok (base, args),
        options: [],
        parameters: [],
        subcommands: NoSubcommands,
    }

getValuesForOption : CliBuilder state s, OptionConfig, List Arg -> Result { out : state, values : List ArgValue, remainingArgs : List Arg } ArgExtractErr
getValuesForOption = \@CliBuilder builder, option, args ->
    subcommandNames = getSubcommandNames builder.subcommands
    (out, restOfArgs) <- builder.parser args
        |> Result.try
    { values, remainingArgs, subcommandFound: _ } <- extractOptionValues { args: restOfArgs, option, subcommandNames }
        |> Result.try

    Ok { out, values, remainingArgs }

updateBuilder : CliBuilder (a -> state) s, ValueParser state, OptionConfig -> CliBuilder state s
updateBuilder = \@CliBuilder builder, parser, option ->
    @CliBuilder {
        parameters: builder.parameters,
        options: builder.options |> List.append option,
        subcommands: builder.subcommands,
        parser,
    }

numOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (U64 -> state) s -> CliBuilder state s)
numOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Num, plurality: One, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { out, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            numValue <- getSingleValue values option
                |> Result.try \val -> parseNumValue val option
                |> Result.try

            Ok (out numValue, remainingArgs)

        updateBuilder builder newParser option

strOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Str -> state) s -> CliBuilder state s)
strOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Str, plurality: One, argsNeeded: One, short, long, name, help }

    \builder ->
        newParser = \args ->
            { out, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getSingleValue values option
                |> Result.try

            when value is
                Ok val -> Ok (out val, remainingArgs)
                Err NoValue -> Err (NoValueProvidedForOption option)

        updateBuilder builder newParser option

flagOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (Bool -> state) s -> CliBuilder state s)
flagOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Bool, plurality: Optional, argsNeeded: Zero, short, long, name, help }

    \builder ->
        newParser = \args ->
            { out, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try
            value <- getOptionalValue values option
                |> Result.try

            when value is
                Err NoValue -> Ok (out Bool.false, remainingArgs)
                Ok (Err NoValue) -> Ok (out Bool.true, remainingArgs)
                Ok (Ok _val) -> Err (OptionDoesNotExpectValue option)

        updateBuilder builder newParser option

occurrenceOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder (U64 -> state) s -> CliBuilder state s)
occurrenceOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option = { type: Bool, plurality: Many, argsNeeded: Zero, short, long, name, help }

    \builder ->
        newParser = \args ->
            { out, values, remainingArgs } <- getValuesForOption builder option args
                |> Result.try

            if values |> List.any Result.isOk then
                Err (OptionDoesNotExpectValue option)
            else
                Ok (out (List.len values), remainingArgs)

        updateBuilder builder newParser option

getParser : CliBuilder state s -> (List Str -> Result (state, Result s [NoSubcommand]) ArgExtractErr)
getParser = \@CliBuilder builder ->
    \args ->
        parsedArgs <- parseArgs args
            |> Result.mapErr FailedToParseArgs
            |> Result.try
        (state, _remainingArgs) <- builder.parser parsedArgs
            |> Result.try

        # TODO allow ensuring no unknown args were passed
        Ok (state, Err NoSubcommand)

expect
    parser =
        cliBuilder {
            alpha: <- numOption { short: "a" },
            beta: <- flagOption { short: "b", long: "--beta" },
            xyz: <- strOption { long: "xyz" },
            verbosity: <- occurrenceOption { short: "v", long: "--verbose" },
        }
        |> getParser

    out = parser ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv"]

    out == Ok ({ alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4 }, Err NoSubcommand)
