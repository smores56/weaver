module [extract_param_values, extract_option_values]

import Arg exposing [Arg]
import Base exposing [ArgExtractErr, OptionConfig, ParameterConfig]
import Parser exposing [ParsedArg, ArgValue]

ExtractParamValuesParams : {
    args : List ParsedArg,
    param : ParameterConfig,
}

ExtractParamValuesState : {
    action : [GetParam, StopParsing],
    values : List Arg,
    remaining_args : List ParsedArg,
}

ExtractParamValuesOutput : {
    values : List Arg,
    remaining_args : List ParsedArg,
}

extract_param_values : ExtractParamValuesParams -> Result ExtractParamValuesOutput ArgExtractErr
extract_param_values = \{ args, param } ->
    starting_state = {
        action: GetParam,
        values: [],
        remaining_args: [],
    }

    state_after =
        args
        |> List.walkTry starting_state \state, arg ->
            when state.action is
                GetParam -> extract_single_param state param arg
                StopParsing -> Ok { state & remaining_args: state.remaining_args |> List.append arg }

    Result.map state_after \{ values, remaining_args } ->
        { values, remaining_args }

extract_single_param : ExtractParamValuesState, ParameterConfig, ParsedArg -> Result ExtractParamValuesState ArgExtractErr
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
    args : List ParsedArg,
    option : OptionConfig,
}

ExtractOptionValuesOutput : {
    values : List ArgValue,
    remaining_args : List ParsedArg,
}

ExtractOptionValueWalkerState : {
    action : [FindOption, GetValue],
    values : List ArgValue,
    remaining_args : List ParsedArg,
}

extract_option_values : ExtractOptionValuesParams -> Result ExtractOptionValuesOutput ArgExtractErr
extract_option_values = \{ args, option } ->
    starting_state = {
        action: FindOption,
        values: [],
        remaining_args: [],
    }

    state_after = List.walkTry args starting_state \state, arg ->
        when state.action is
            FindOption -> find_option_for_extraction state arg option
            GetValue -> get_value_for_extraction state arg option

    when state_after is
        Err err -> Err err
        Ok { action, values, remaining_args } ->
            when action is
                GetValue -> Err (NoValueProvidedForOption option)
                FindOption -> Ok { values, remaining_args }

find_option_for_extraction : ExtractOptionValueWalkerState, ParsedArg, OptionConfig -> Result ExtractOptionValueWalkerState ArgExtractErr
find_option_for_extraction = \state, arg, option ->
    when arg is
        Short short ->
            if short == option.short then
                if option.expected_value == NothingExpected then
                    Ok { state & values: state.values |> List.append (Err NoValue) }
                else
                    Ok { state & action: GetValue }
            else
                Ok { state & remaining_args: state.remaining_args |> List.append arg }

        ShortGroup short_group ->
            find_options_in_short_group state option short_group

        Long long ->
            if long.name == option.long then
                if option.expected_value == NothingExpected then
                    when long.value is
                        Ok _val -> Err (OptionDoesNotExpectValue option)
                        Err NoValue -> Ok { state & values: state.values |> List.append (Err NoValue) }
                else
                    when long.value is
                        Ok val -> Ok { state & values: state.values |> List.append (Ok val) }
                        Err NoValue -> Ok { state & action: GetValue }
            else
                Ok { state & remaining_args: state.remaining_args |> List.append arg }

        _nothing_found ->
            Ok { state & remaining_args: state.remaining_args |> List.append arg }

find_options_in_short_group : ExtractOptionValueWalkerState, OptionConfig, { names : List Str, complete : [Partial, Complete] } -> Result ExtractOptionValueWalkerState ArgExtractErr
find_options_in_short_group = \state, option, short_group ->
    state_after =
        short_group.names
        |> List.walkTry { action: FindOption, remaining: [], values: [] } \sg_state, name ->
            when sg_state.action is
                GetValue -> Err (CannotUsePartialShortGroupAsValue option short_group.names)
                FindOption ->
                    if name == option.short then
                        if option.expected_value == NothingExpected then
                            Ok { sg_state & values: sg_state.values |> List.append (Err NoValue) }
                        else
                            Ok { sg_state & action: GetValue }
                    else
                        Ok sg_state

    when state_after is
        Err err -> Err err
        Ok { action, remaining, values } ->
            rest_of_group =
                if List.isEmpty values then
                    Ok (ShortGroup short_group)
                else if List.isEmpty remaining then
                    Err NoValue
                else
                    Ok (ShortGroup { complete: Partial, names: remaining })

            Ok
                { state &
                    action,
                    remaining_args: state.remaining_args |> List.appendIfOk rest_of_group,
                    values: state.values |> List.concat values,
                }

get_value_for_extraction : ExtractOptionValueWalkerState, ParsedArg, OptionConfig -> Result ExtractOptionValueWalkerState ArgExtractErr
get_value_for_extraction = \state, arg, option ->
    value =
        when arg is
            Short s -> Arg.from_str "-$(s)"
            ShortGroup { names, complete: Complete } -> Arg.from_str "-$(Str.joinWith names "")"
            ShortGroup { names, complete: Partial } ->
                return Err (CannotUsePartialShortGroupAsValue option names)

            Long { name, value: Ok val } ->
                # Using [Arg.display] is safe here because `val` must be valid UTF-8 to be `Ok`
                Arg.from_str "--$(name)=$(Arg.display val)"

            Long { name, value: Err NoValue } -> Arg.from_str "--$(name)"
            Parameter p -> p

    Ok { state & action: FindOption, values: state.values |> List.append (Ok value) }
