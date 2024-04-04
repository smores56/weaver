interface Builder
    exposes [strOption, numOption, flagOption, occurrenceOption, cliBuilder, getParser]
    imports [
        Config.{
            ValueType,
            Plurality,
            OptionConfig,
            OptionConfigParams,
            ParameterConfig,
            SubcommandConfigParams,
            SubcommandsConfig,
            getSubcommandNames,
            CliConfigParams,
            CliConfig,
        },
        Parser.{ Arg, ParsedArgs, ArgParseErr, parseArgs },
    ]

# TODO: make a separate CliBuilderErr for building issues (e.g. duplicate arg names)
# and a ArgExtractErr for missing args, invalid args, etc.
CliBuilderErr : [
    MissingArg OptionConfig,
    OptionCanOnlyBeSetOnce OptionConfig,
    NoValueProvidedForOption OptionConfig,
    TooManyValuesForArg OptionConfig,
    CannotUseGroupedShortArgAsValue OptionConfig Arg,
    InvalidNumArg OptionConfig,
    FailedToParseArgs ArgParseErr,
]

# This should return an ArgExtractErr
ValueParser in out : in, List Arg -> Result (out, List Arg) CliBuilderErr

CliBuilder base state := {
    baseBuilder : base,
    builder : ValueParser base state,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : SubcommandsConfig,
}

ArgValue : Result Str [NoValue]

cliBuilder : base -> CliBuilder base base
cliBuilder = \base ->
    @CliBuilder {
        baseBuilder: base,
        builder: \state, args -> Ok (state, args),
        options: [],
        parameters: [],
        subcommands: NoSubcommands,
    }

getValuesOfArgs : { args : List Arg, option : OptionConfig, subcommandNames : List Str } -> Result (List ArgValue, List Arg) CliBuilderErr
getValuesOfArgs = \{ args, option, subcommandNames: _ } ->
    # TODO: add StopParsing action
    # TODO: only check if _first_ parameter is subcommand, ignore rest
    stateAfter =
        args
        |> List.walkTry { action: FindOption, values: [], remainingArgs: [] } \state, arg ->
            when state.action is
                FindOption ->
                    when arg is
                        Short s ->
                            if s.name == option.short then
                                when option.argsNeeded is
                                    Zero ->
                                        Ok { state & values: state.values |> List.append (Err NoValue) }

                                    One ->
                                        Ok { state & action: GetValue }
                            else
                                Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

                        Long l ->
                            if l.name == option.long then
                                when option.argsNeeded is
                                    Zero ->
                                        when l.value is
                                            Ok _val -> Err (TooManyValuesForArg option)
                                            Err NoValue -> Ok { state & values: state.values |> List.append (Err NoValue) }

                                    One ->
                                        when l.value is
                                            Ok val -> Ok { state & values: state.values |> List.append (Ok val) }
                                            Err NoValue -> Ok { state & action: GetValue }
                            else
                                Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

                        # TODO: detect subcommands
                        Parameter _p ->
                            # if subcommandNames |> List.contains p then
                            #     Ok { state & }
                            Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

                GetValue ->
                    when arg is
                        Short s ->
                            when s.grouped is
                                Alone ->
                                    Ok { state & action: FindOption, values: state.values |> List.append (Ok "-$(s.name)") }

                                Grouped ->
                                    Err (CannotUseGroupedShortArgAsValue option arg)

                        Long l ->
                            value =
                                when l.value is
                                    Ok val -> "--$(l.name)=$(val)"
                                    Err NoValue -> "--$(l.name)"

                            Ok { state & action: FindOption, values: state.values |> List.append (Ok value) }

                        # TODO: detect subcommands
                        Parameter p ->
                            Ok { state & action: FindOption, values: state.values |> List.append (Ok p) }

    when stateAfter is
        Err err -> Err err
        Ok { action, values, remainingArgs } ->
            if action == GetValue then
                Err (NoValueProvidedForOption option)
            else
                Ok (values, remainingArgs)

getValuesForOption : CliBuilder base (in -> state), OptionConfig, List Arg -> Result { out : in -> state, values : List (Result Str [NoValue]), remainingArgs : List Arg } CliBuilderErr
getValuesForOption = \@CliBuilder builder, option, args ->
    subcommandNames = getSubcommandNames builder.subcommands
    (out, restOfArgs) <- builder.builder builder.baseBuilder args
        |> Result.try
    (values, remainingArgs) <- getValuesOfArgs { args: restOfArgs, option, subcommandNames }
        |> Result.try

    Ok { out, values, remainingArgs }

numOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder base (U64 -> state) -> CliBuilder base state)
numOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option : OptionConfig
    option = { type: Num, plurality: One, argsNeeded: One, short, long, name, help }

    \@CliBuilder builder ->
        newBuilder = \_state, args ->
            { out, values, remainingArgs } <- getValuesForOption (@CliBuilder builder) option args
                |> Result.try

            when values is
                [] -> Err (MissingArg option)
                [single] ->
                    val <- single
                        |> Result.mapErr \_ -> NoValueProvidedForOption option
                        |> Result.try
                    num <- val
                        |> Str.toU64
                        |> Result.mapErr \_ -> InvalidNumArg option
                        |> Result.try

                    Ok (out num, remainingArgs)

                _many -> Err (OptionCanOnlyBeSetOnce option)

        @CliBuilder {
            baseBuilder: builder.baseBuilder,
            parameters: builder.parameters,
            options: builder.options |> List.append option,
            subcommands: builder.subcommands,
            builder: newBuilder,
        }

strOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder base (Str -> state) -> CliBuilder base state)
strOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option : OptionConfig
    option = { type: Str, plurality: One, argsNeeded: One, short, long, name, help }

    \@CliBuilder builder ->
        newBuilder = \_state, args ->
            { out, values, remainingArgs } <- getValuesForOption (@CliBuilder builder) option args
                |> Result.try

            when values is
                [] -> Err (MissingArg option)
                [single] ->
                    val <- single
                        |> Result.mapErr \_ -> NoValueProvidedForOption option
                        |> Result.try

                    Ok (out val, remainingArgs)

                _many -> Err (OptionCanOnlyBeSetOnce option)

        @CliBuilder {
            baseBuilder: builder.baseBuilder,
            parameters: builder.parameters,
            options: builder.options |> List.append option,
            subcommands: builder.subcommands,
            builder: newBuilder,
        }

flagOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder base (Bool -> state) -> CliBuilder base state)
flagOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option : OptionConfig
    option = { type: Bool, plurality: Optional, argsNeeded: Zero, short, long, name, help }

    \@CliBuilder builder ->
        newBuilder = \_state, args ->
            { out, values, remainingArgs } <- getValuesForOption (@CliBuilder builder) option args
                |> Result.try

            when values is
                [] -> Ok (out Bool.false, remainingArgs)
                [Err NoValue] -> Ok (out Bool.true, remainingArgs)
                [Ok _val] -> Err (TooManyValuesForArg option)
                _many -> Err (OptionCanOnlyBeSetOnce option)

        @CliBuilder {
            baseBuilder: builder.baseBuilder,
            parameters: builder.parameters,
            options: builder.options |> List.append option,
            subcommands: builder.subcommands,
            builder: newBuilder,
        }

occurrenceOption : { short ? Str, long ? Str, name ? Str, help ? Str } -> (CliBuilder base (U64 -> state) -> CliBuilder base state)
occurrenceOption = \{ short ? "", long ? "", name ? "", help ? "" } ->
    option : OptionConfig
    option = { type: Bool, plurality: Many, argsNeeded: Zero, short, long, name, help }

    \@CliBuilder builder ->
        newBuilder = \_state, args ->
            { out, values, remainingArgs } <- getValuesForOption (@CliBuilder builder) option args
                |> Result.try

            if values |> List.any Result.isOk then
                Err (TooManyValuesForArg option)
            else
                Ok (out (List.len values), remainingArgs)

        @CliBuilder {
            baseBuilder: builder.baseBuilder,
            parameters: builder.parameters,
            options: builder.options |> List.append option,
            subcommands: builder.subcommands,
            builder: newBuilder,
        }

getParser : CliBuilder base {}state -> (List Str -> Result {}state CliBuilderErr)
getParser = \@CliBuilder builder ->
    \args ->
        parsedArgs <- parseArgs args
            |> Result.mapErr FailedToParseArgs
            |> Result.try

        builder.baseBuilder
        |> builder.builder parsedArgs
        |> Result.map .0

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

    out == Ok { alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4 }
