interface Extract
    exposes [
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

ExtractParamValuesParams : {
    args : List Arg,
    param : ParameterConfig,
}

ExtractParamValuesOutput : {
    values : List Str,
    remainingArgs : List Arg,
}

extractParamValues : ExtractParamValuesParams -> Result ExtractParamValuesOutput ArgExtractErr
extractParamValues = \{ args, param } ->
    startingState = {
        action: GetParam,
        values: [],
        remainingArgs: [],
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
                            if p == "--" then
                                Ok { state & action: PassThrough }
                            else
                                when param.plurality is
                                    Optional | One -> Ok { state & action: StopParsing, values: state.values |> List.append p }
                                    Many -> Ok { state & values: state.values |> List.append p }

                PassThrough ->
                    value =
                        when arg is
                            Short s -> "-$(s)"
                            ShortGroup sg -> "-$(Str.joinWith sg.names "")"
                            Long { name, value: Ok val } -> "--$(name)=$(val)"
                            Long { name, value: Err NoValue } -> "--$(name)"
                            Parameter p -> p

                    Ok { state & remainingArgs: state.remainingArgs |> List.append (Parameter value) }

                StopParsing ->
                    Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

    Result.map stateAfter \{ values, remainingArgs } -> { values, remainingArgs }

ExtractOptionValuesParams : {
    args : List Arg,
    option : OptionConfig,
}

ExtractOptionValuesOutput : {
    values : List ArgValue,
    remainingArgs : List Arg,
}

ExtractOptionValueWalkerState : {
    action : [FindOption, GetValue, StopParsing],
    values : List ArgValue,
    remainingArgs : List Arg,
}

extractOptionValues : ExtractOptionValuesParams -> Result ExtractOptionValuesOutput ArgExtractErr
extractOptionValues = \{ args, option } ->
    startingState = {
        action: FindOption,
        values: [],
        remainingArgs: [],
    }

    stateAfter = List.walkTry args startingState \state, arg ->
        when state.action is
            FindOption -> findOptionForExtraction state arg option
            GetValue -> getValueForExtraction state arg option
            StopParsing -> Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

    when stateAfter is
        Err err -> Err err
        Ok { action, values, remainingArgs } ->
            when action is
                GetValue -> Err (NoValueProvidedForOption option)
                FindOption | StopParsing -> Ok { values, remainingArgs }

findOptionForExtraction : ExtractOptionValueWalkerState, Arg, OptionConfig -> Result ExtractOptionValueWalkerState ArgExtractErr
findOptionForExtraction = \state, arg, option ->
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
                        if List.isEmpty values then
                            Ok (ShortGroup shortGroup)
                        else if List.isEmpty remaining then
                            Err NoValue
                        else
                            Ok (ShortGroup { complete: Partial, names: remaining })

                    Ok
                        { state &
                            action: newAction,
                            remainingArgs: state.remainingArgs |> List.appendIfOk restOfGroup,
                            values: state.values |> List.concat values,
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

        _nothingFound ->
            Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

getValueForExtraction : ExtractOptionValueWalkerState, Arg, OptionConfig -> Result ExtractOptionValueWalkerState ArgExtractErr
getValueForExtraction = \state, arg, option ->
    value =
        when arg is
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
