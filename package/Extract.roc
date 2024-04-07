interface Extract
    exposes [
        SubcommandSearchResult,
        ExtractParamValuesParams,
        ExtractParamValuesOutput,
        extractParamValues,
        ExtractOptionValuesParams,
        ExtractOptionValuesOutput,
        extractOptionValues,
        getOptionalValue,
        getSingleValue,
    ]
    imports [
        Parser.{ Arg, ArgValue, ArgParseErr },
        Config.{ OptionConfig, ParameterConfig, ArgExtractErr, DataParser, SubcommandConfig },
    ]

SubcommandSearchResult s ss :
    Result
    {
        name : Str,
        parser : DataParser s ss,
        config : SubcommandConfig,
        args : List Arg,
    }
    [NoSubcommand]

ExtractParamValuesParams s ss : {
    args : List Arg,
    param : ParameterConfig,
    subcommands : Dict Str { parser : DataParser s ss, config : SubcommandConfig },
}

ExtractParamValuesOutput s ss : {
    values : List Str,
    remainingArgs : List Arg,
    subcommandFound : SubcommandSearchResult s ss,
}

extractParamValues : ExtractParamValuesParams s ss -> Result (ExtractParamValuesOutput s ss) ArgExtractErr
extractParamValues = \{ args, param, subcommands } ->
    startingState = {
        action: GetParam,
        checkForSubcommand: Check,
        values: [],
        remainingArgs: [],
        subcommandFound: Err NoSubcommand,
    }

    stateAfter =
        args
        |> List.walkTry startingState \state, arg ->
            when state.action is
                GetParam ->
                    when arg is
                        Short short ->
                            Err (UnrecognizedShortArg short)

                        ShortGroup group ->
                            Err (UnrecognizedShortArg (List.first group.names |> Result.withDefault ""))

                        Long long ->
                            Err (UnrecognizedLongArg long.name)

                        Parameter p ->
                            # TODO: pass args through for subcommands when necessary
                            when (state.checkForSubcommand, Dict.get subcommands p) is
                                (Check, Ok subcommand) ->
                                    Ok { state & action: StopParsing, subcommandFound: Ok { name: p, config: subcommand.config, parser: subcommand.parser, args: [] } }

                                _other ->
                                    if p == "--" then
                                        Ok { state & action: PassThrough }
                                    else
                                        when param.plurality is
                                            Optional | One -> Ok { state & action: StopParsing, values: state.values |> List.append p }
                                            Many -> Ok { state & checkForSubcommand: DontCheck, values: state.values |> List.append p }

                PassThrough ->
                    value = when arg is
                        Short s -> "-$(s)"
                        ShortGroup sg -> "-$(Str.joinWith sg.names "")"
                        Long { name, value: Ok val } -> "--$(name)=$(val)"
                        Long { name, value: Err NoValue } -> "--$(name)"
                        Parameter p -> p

                    Ok { state & remainingArgs: state.remainingArgs |> List.append (Parameter value) }

                StopParsing ->
                    Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

    stateAfter
    |> Result.map \{ values, remainingArgs, subcommandFound } ->
        { values, remainingArgs, subcommandFound }

ExtractOptionValuesParams s ss : {
    args : List Arg,
    option : OptionConfig,
    subcommands : Dict Str { parser : DataParser s ss, config : SubcommandConfig },
}

ExtractOptionValuesOutput s ss : {
    values : List ArgValue,
    remainingArgs : List Arg,
    subcommandFound : SubcommandSearchResult s ss,
}

ExtractOptionValueWalkerState s ss : {
    action : [FindOption, GetValue, StopParsing],
    checkForSubcommand : [Check, DontCheck],
    values : List ArgValue,
    remainingArgs : List Arg,
    subcommandFound : SubcommandSearchResult s ss,
}

extractOptionValues : ExtractOptionValuesParams s ss -> Result (ExtractOptionValuesOutput s ss) ArgExtractErr
extractOptionValues = \{ args, option, subcommands } ->
    startingState = {
        action: FindOption,
        checkForSubcommand: Check,
        values: [],
        remainingArgs: [],
        subcommandFound: Err NoSubcommand,
    }

    stateAfter = List.walkTry args startingState \state, arg ->
        when state.action is
            FindOption -> findOptionForExtraction state arg option subcommands
            GetValue -> getValueForExtraction state arg option
            StopParsing ->
                # TODO: do we have to pass remainingArgs along normally if there's no subcommand?
                subcommandFound =
                    state.subcommandFound 
                    |> Result.map \sf -> { sf & args: sf.args |> List.append arg }

                Ok { state & subcommandFound }

    when stateAfter is
        Err err -> Err err
        Ok { action, values, remainingArgs, subcommandFound } ->
            when action is
                GetValue -> Err (NoValueProvidedForOption option)
                FindOption | StopParsing -> Ok { values, remainingArgs, subcommandFound }

findOptionForExtraction :
    ExtractOptionValueWalkerState s ss,
    Arg,
    OptionConfig,
    Dict Str {
        parser : DataParser s ss,
        config : SubcommandConfig,
    }
    -> Result (ExtractOptionValueWalkerState s ss) ArgExtractErr
findOptionForExtraction = \state, arg, option, subcommands ->
    when arg is
        Short short if short == option.short ->
            when option.argsNeeded is
                Zero ->
                    Ok { state & values: state.values |> List.append (Err NoValue) }

                One ->
                    Ok { state & action: GetValue }

        ShortGroup shortGroup ->
            stateAfter =
                shortGroup.names
                |> List.walkTry { action: FindOption, remaining: [], values: [] } \sgState, name ->
                    when sgState.action is
                        GetValue -> Err (CannotUsePartialShortGroupAsValue option shortGroup.names)
                        FindOption ->
                            if name == option.short then
                                when option.argsNeeded is
                                    Zero -> Ok { sgState & values: sgState.values |> List.append (Err NoValue) }
                                    One -> Ok { sgState & action: GetValue }
                            else
                                Ok sgState

            when stateAfter is
                Err err -> Err err
                Ok { action, remaining, values } ->
                    newAction =
                        when action is
                            GetValue -> GetValue
                            FindOption -> FindOption
                    restOfGroup =
                        if List.isEmpty remaining then
                            Err NoValue
                        else if List.isEmpty values then
                            Ok (ShortGroup shortGroup)
                        else
                            Ok (ShortGroup { complete: Partial, names: remaining })

                    Ok
                        { state &
                            action: newAction,
                            remainingArgs: state.remainingArgs |> List.appendIfOk restOfGroup,
                            values: state.values |> List.concat values
                        }

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
            when Dict.get subcommands param is
                Err KeyNotFound ->
                    Ok { state & checkForSubcommand: DontCheck, remainingArgs: state.remainingArgs |> List.append arg }

                Ok { parser, config } ->
                    Ok
                        { state &
                            action: StopParsing,
                            subcommandFound: Ok { name: param, parser, config, args: [arg] },
                            remainingArgs: state.remainingArgs |> List.append arg,
                        }

        _nothingFound ->
            Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

getValueForExtraction : ExtractOptionValueWalkerState s ss, Arg, OptionConfig -> Result (ExtractOptionValueWalkerState s ss) ArgExtractErr
getValueForExtraction = \state, arg, option ->
    value = when arg is
        Short s -> Ok "-$(s)"
        ShortGroup { names, complete: Complete } -> Ok "-$(Str.joinWith names "")"
        ShortGroup { names, complete: Partial } -> Err (CannotUsePartialShortGroupAsValue option names)
        Long { name, value: Ok val } -> Ok "--$(name)=$(val)"
        Long { name, value: Err NoValue } -> Ok "--$(name)"
        Parameter p -> Ok p

    value
    |> Result.map \val ->
        { state & action: FindOption, values: state.values |> List.append (Ok val) }

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
