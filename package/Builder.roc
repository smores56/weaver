interface Builder
    exposes [
        cliBuilder,
        finishOrErr,
        numOption,
        strOption,
        flagOption,
        occurrenceOption,
    ]
    imports [
        Option.{ CliOption, OptionBase, OptionName, optionName },
        Parser.{ Arg, ParsedArgs, ArgParseErr, parseArgs },
    ]

CliBuilderErr : [
    MissingArg Str,
    NoValueProvidedForArg Str,
    TooManyValuesForArg Str,
    InvalidNumArg Str,
    FailedToParseArgs ArgParseErr,
]

CliBuilderConfig : {
    name : Str,
    authors ? List Str,
    version ? Str,
    description ? Str,
    allowExtraArgs ? [Allow, Deny],
}

CliBuilder state := {
    config : CliBuilderConfig,
    args : List Arg,
    options : List CliOption,
    state : Result state CliBuilderErr,
}

ArgValue : Result Str [NoValue]

ValueParser a : List ArgValue -> Result a CliBuilderErr

cliBuilder : List Str, CliBuilderConfig, state -> CliBuilder state
cliBuilder = \args, config, state ->
    (parsedArgs, stateResult) =
        when parseArgs args is
            Ok parsed -> (parsed, Ok state)
            Err parseErr -> ([], Err (FailedToParseArgs parseErr))

    @CliBuilder { config, args: parsedArgs, options: [], state: stateResult }

unwrapBuilderState : CliBuilder (a -> state), CliOption, ((a -> state) -> CliBuilder state) -> CliBuilder state
unwrapBuilderState = \@CliBuilder builder, option, callback ->
    when builder.state is
        Ok validState -> callback validState
        Err invalidState ->
            @CliBuilder {
                args: builder.args,
                config: builder.config,
                state: Err invalidState,
                options: List.append builder.options option,
            }

updateBuilderViaParser : CliBuilder (a -> state), CliOption, ValueParser a -> CliBuilder state
updateBuilderViaParser = \@CliBuilder builder, option, parser ->
    validState <- unwrapBuilderState (@CliBuilder builder) option

    result =
        getValuesOfArgs builder.args option
        |> Result.try \(values, remainingArgs) ->
            value <- parser values
                |> Result.map

            (value, remainingArgs)

    when result is
        Ok (value, remainingArgs) ->
            @CliBuilder {
                args: remainingArgs,
                config: builder.config,
                state: Ok (validState value),
                options: List.append builder.options option,
            }

        Err parseErr ->
            @CliBuilder {
                args: builder.args,
                config: builder.config,
                state: Err parseErr,
                options: List.append builder.options option,
            }

getValuesOfArgs : List Arg, CliOption -> Result (List ArgValue, List Arg) CliBuilderErr
getValuesOfArgs = \args, option ->
    name = optionName option

    argNameMatches = \arg ->
        when arg is
            Short s -> s.name == name
            Long l -> l.name == name
            Parameter _param -> Bool.false

    expectedAmountOfArgs =
        when option is
            Flag _ | Frequency _ -> Zero
            Str _ | Num _ | MaybeStr _ | MaybeNum _ | Choice _ | StrList _ | NumList _ -> One

    startingState = {
        action: Nothing,
        values: [],
        remainingArgs: [],
    }

    stateAfter =
        args
        |> List.walk startingState \state, arg ->
            when state.action is
                Nothing ->
                    if argNameMatches arg then
                        if expectedAmountOfArgs == Zero then
                            { state & values: state.values |> List.append (Err NoValue) }
                        else
                            { state & action: GetValue }
                    else
                        { state & remainingArgs: state.remainingArgs |> List.append arg }

                GetValue ->
                    value =
                        when arg is
                            Short short -> "-$(short.name)"
                            Long long -> "--$(long.name)"
                            Parameter param -> param

                    { state & action: Nothing, values: state.values |> List.append (Ok value) }

    if stateAfter.action == GetValue then
        Err (NoValueProvidedForArg (optionName option))
    else
        Ok (stateAfter.values, stateAfter.remainingArgs)

getFirstValue : List ArgValue, CliOption -> Result Str CliBuilderErr
getFirstValue = \values, config ->
    if List.isEmpty values then
        Err (MissingArg (optionName config))
    else
        when List.keepOks values \v -> v is
            [] -> Err (NoValueProvidedForArg (optionName config))
            [value] -> Ok value
            [..] -> Err (TooManyValuesForArg (optionName config))

numOption : OptionBase {} -> (CliBuilder (U64 -> state) -> CliBuilder state)
numOption = \config ->
    parser = \values ->
        value <- getFirstValue values config
            |> Result.try

        value
        |> Str.toU64
        |> Result.mapErr \InvalidNumStr -> InvalidNumArg (optionName config)

    \builder -> updateBuilderViaParser builder (Num config) parser

strOption : OptionBase {} -> (CliBuilder (Str -> state) -> CliBuilder state)
strOption = \config ->
    parser = \values ->
        getFirstValue values config

    \builder -> updateBuilderViaParser builder (Str config) parser

flagOption : OptionBase {} -> (CliBuilder (Bool -> state) -> CliBuilder state)
flagOption = \config ->
    parser = \values ->
        Ok (values |> List.isEmpty |> Bool.not)

    \builder -> updateBuilderViaParser builder (Str config) parser

occurrenceOption : OptionBase {} -> (CliBuilder (U64 -> state) -> CliBuilder state)
occurrenceOption = \config ->
    parser = \values ->
        Ok (values |> List.len)

    \builder -> updateBuilderViaParser builder (Str config) parser

finishOrErr : CliBuilder state -> Result state CliBuilderErr
finishOrErr = \@CliBuilder builder -> builder.state

# TODO: ensure the right quantities of args
# expectedFrequency =
#     when option is
#         Str _ | Num _ -> One
#         Flag _ | MaybeStr _ | MaybeNum _ | Choice _ -> One -> ZeroOrOne
#         StrList _ | NumList _ | Frequency _ -> Any

expect
    args = ["app", "-a", "123", "-b", "--xyz", "some_text"]
    out =
        cliBuilder args { name: "app" } {
            a: <- numOption { short: "a" },
            b: <- flagOption { short: "b" },
            xyz: <- strOption { long: "some_text" },
        }
        |> finishOrErr

    out == Ok { a: 123, b: Bool.true, xyz: "some_text" }
