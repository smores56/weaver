module [extractParamValues, extractOptionValues]

import Base exposing [ArgExtractErr, OptionConfig, ParameterConfig]
import Parser exposing [Arg, ArgValue]

ExtractParamValuesParams : {
    args : List Arg,
    param : ParameterConfig,
}

ExtractParamValuesState : {
    action : [GetParam, StopParsing],
    values : List Str,
    remainingArgs : List Arg,
}

ExtractParamValuesOutput : {
    values : List Str,
    remainingArgs : List Arg,
}

extract_param_values : ExtractParamValuesParams -> Result ExtractParamValuesOutput ArgExtractErr
extract_param_values = \{ args, param } ->
    starting_state = {
        action: GetParam,
        values: [],
        remainingArgs: [],
    }

    state_after =
        args
        |> List.walkTry startingState \state, arg ->
            when state.action is
                GetParam -> extractSingleParam state param arg
                StopParsing -> Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

    Result.map stateAfter \{ values, remaining_args } ->
        { values, remainingArgs }

extract_single_param : ExtractParamValuesState, ParameterConfig, Arg -> Result ExtractParamValuesState ArgExtractErr
extract_single_param = \state, param, arg ->
    when arg is
        Short short ->
            Err (UnrecognizedShortArg short)

        ShortGroup group ->
            name =
                group.names
                |> List.first
                |> Result.withDefault ""

            Err (UnrecognizedShortArg name)

        Long long ->
            Err (UnrecognizedLongArg long.name)

        Parameter p ->
            when param.plurality is
                Optional | One -> Ok { state & action: StopParsing, values: state.values |> List.append p }
                Many -> Ok { state & values: state.values |> List.append p }

ExtractOptionValuesParams : {
    args : List Arg,
    option : OptionConfig,
}

ExtractOptionValuesOutput : {
    values : List ArgValue,
    remainingArgs : List Arg,
}

ExtractOptionValueWalkerState : {
    action : [FindOption, GetValue],
    values : List ArgValue,
    remainingArgs : List Arg,
}

extract_option_values : ExtractOptionValuesParams -> Result ExtractOptionValuesOutput ArgExtractErr
extract_option_values = \{ args, option } ->
    starting_state = {
        action: FindOption,
        values: [],
        remainingArgs: [],
    }

    state_after = List.walkTry args startingState \state, arg ->
        when state.action is
            FindOption -> findOptionForExtraction state arg option
            GetValue -> getValueForExtraction state arg option

    when stateAfter is
        Err err -> Err err
        Ok { action, values, remaining_args } ->
            when action is
                GetValue -> Err (NoValueProvidedForOption option)
                FindOption -> Ok { values, remainingArgs }

find_option_for_extraction : ExtractOptionValueWalkerState, Arg, OptionConfig -> Result ExtractOptionValueWalkerState ArgExtractErr
find_option_for_extraction = \state, arg, option ->
    when arg is
        Short short ->
            if short == option.short then
                if option.expectedValue == NothingExpected then
                    Ok { state & values: state.values |> List.append (Err NoValue) }
                else
                    Ok { state & action: GetValue }
            else
                Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

        ShortGroup short_group ->
            findOptionsInShortGroup state option shortGroup

        Long long ->
            if long.name == option.long then
                if option.expectedValue == NothingExpected then
                    when long.value is
                        Ok _val -> Err (OptionDoesNotExpectValue option)
                        Err NoValue -> Ok { state & values: state.values |> List.append (Err NoValue) }
                else
                    when long.value is
                        Ok val -> Ok { state & values: state.values |> List.append (Ok val) }
                        Err NoValue -> Ok { state & action: GetValue }
            else
                Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

        _nothingFound ->
            Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

find_options_in_short_group : ExtractOptionValueWalkerState, OptionConfig, { names : List Str, complete : [Partial, Complete] } -> Result ExtractOptionValueWalkerState ArgExtractErr
find_options_in_short_group = \state, option, short_group ->
    state_after =
        shortGroup.names
        |> List.walkTry { action: FindOption, remaining: [], values: [] } \sg_state, name ->
            when sgState.action is
                GetValue -> Err (CannotUsePartialShortGroupAsValue option shortGroup.names)
                FindOption ->
                    if name == option.short then
                        if option.expectedValue == NothingExpected then
                            Ok { sgState & values: sgState.values |> List.append (Err NoValue) }
                        else
                            Ok { sgState & action: GetValue }
                    else
                        Ok sgState

    when stateAfter is
        Err err -> Err err
        Ok { action, remaining, values } ->
            rest_of_group =
                if List.isEmpty values then
                    Ok (ShortGroup shortGroup)
                else if List.isEmpty remaining then
                    Err NoValue
                else
                    Ok (ShortGroup { complete: Partial, names: remaining })

            Ok
                { state &
                    action,
                    remainingArgs: state.remainingArgs |> List.appendIfOk restOfGroup,
                    values: state.values |> List.concat values,
                }

get_value_for_extraction : ExtractOptionValueWalkerState, Arg, OptionConfig -> Result ExtractOptionValueWalkerState ArgExtractErr
get_value_for_extraction = \state, arg, option ->
    value =
        when arg is
            Short s -> Ok "-$(s)"
            ShortGroup { names, complete: Complete } -> Ok "-$(Str.joinWith names "")"
            ShortGroup { names, complete: Partial } -> Err (CannotUsePartialShortGroupAsValue option names)
            Long { name, value: Ok val } -> Ok "--$(name)=$(val)"
            Long { name, value: Err NoValue } -> Ok "--$(name)"
            Parameter p -> Ok p

    Result.map value \val ->
        { state & action: FindOption, values: state.values |> List.append (Ok val) }
