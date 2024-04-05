interface Extract
    exposes [
        ArgExtractErr,
        ExtractValuesParams,
        ExtractValuesOutput,
        extractOptionValues,
        getOptionalValue,
        getSingleValue,
        parseNumValue,
        parseOptionalNumValue,
    ]
    imports [
        Parser.{ Arg, ArgValue, ArgParseErr },
        Config.{ OptionConfig },
    ]

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

ExtractValuesParams : {
    args : List Arg,
    option : OptionConfig,
    subcommandNames : List Str,
}

ExtractValuesOutput : {
    values : List ArgValue,
    remainingArgs : List Arg,
    subcommandFound : ArgValue,
}

ExtractOptionValueWalkerState : {
    action : [FindOption, GetValue, StopParsing],
    checkForSubcommand : [Check, DontCheck],
    values : List ArgValue,
    remainingArgs : List Arg,
    subcommandFound : ArgValue,
}

extractOptionValues : ExtractValuesParams -> Result ExtractValuesOutput ArgExtractErr
extractOptionValues = \{ args, option, subcommandNames } ->
    startingState = {
        action: FindOption,
        checkForSubcommand: Check,
        values: [],
        remainingArgs: [],
        subcommandFound: Err NoValue,
    }

    stateAfter =
        args
        |> List.walkTry startingState \state, arg ->
            when state.action is
                FindOption -> findOptionForExtraction state arg option subcommandNames
                GetValue -> getValueForExtraction state arg option
                StopParsing ->
                    Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

    when stateAfter is
        Err err -> Err err
        Ok { action, values, remainingArgs, subcommandFound } ->
            when action is
                GetValue -> Err (NoValueProvidedForOption option)
                FindOption | StopParsing -> Ok { values, remainingArgs, subcommandFound }

findOptionForExtraction : ExtractOptionValueWalkerState, Arg, OptionConfig, List Str -> Result ExtractOptionValueWalkerState ArgExtractErr
findOptionForExtraction = \state, arg, option, subcommandNames ->
    when arg is
        Short short if short.name == option.short ->
            when option.argsNeeded is
                Zero ->
                    Ok { state & values: state.values |> List.append (Err NoValue) }

                One ->
                    Ok { state & action: GetValue }

        Long long if long.name == option.long ->
            when option.argsNeeded is
                Zero ->
                    when long.value is
                        Ok _val -> Err (OptionDoesNotExpectValue option)
                        Err NoValue -> Ok { state & values: state.values |> List.append (Err NoValue) }

                One ->
                    when long.value is
                        Ok val -> Ok { state & values: state.values |> List.append (Ok val) }
                        Err NoValue -> Ok { state & action: GetValue }

        Parameter param if state.checkForSubcommand == Check ->
            if subcommandNames |> List.contains param then
                Ok { state & action: StopParsing, remainingArgs: state.remainingArgs |> List.append arg }
            else
                Ok { state & checkForSubcommand: DontCheck, remainingArgs: state.remainingArgs |> List.append arg }

        _nothingFound ->
            Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

getValueForExtraction : ExtractOptionValueWalkerState, Arg, OptionConfig -> Result ExtractOptionValueWalkerState ArgExtractErr
getValueForExtraction = \state, arg, option ->
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

        Parameter p ->
            Ok { state & action: FindOption, values: state.values |> List.append (Ok p) }

getSingleValue : List ArgValue, OptionConfig -> Result ArgValue ArgExtractErr
getSingleValue = \values, option ->
    when values is
        [] -> Err (MissingArg option)
        [single] -> Ok single
        [..] -> Err (OptionCanOnlyBeSetOnce option)

getOptionalValue : List ArgValue, OptionConfig -> Result (Result ArgValue [NoValue]) ArgExtractErr
getOptionalValue = \values, option ->
    when values is
        [] -> Ok (Ok (Err NoValue))
        [single] -> Ok (Ok single)
        [..] -> Err (OptionCanOnlyBeSetOnce option)

parseNumValue : ArgValue, OptionConfig -> Result I64 ArgExtractErr
parseNumValue = \value, option ->
    val <- value
        |> Result.mapErr \_ -> NoValueProvidedForOption option
        |> Result.try

    val
    |> Str.toI64
    |> Result.mapErr \_ -> InvalidNumArg option

parseOptionalNumValue : ArgValue, OptionConfig -> Result (Result I64 [NoValue]) ArgExtractErr
parseOptionalNumValue = \value, option ->
    when value is
        Ok val ->
            when Str.toI64 val is
                Ok numVal -> Ok (Ok numVal)
                Err _ -> Err (InvalidNumArg option)

        Err NoValue -> Ok (Err NoValue)

