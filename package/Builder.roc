module [
    GetOptionsAction,
    GetParamsAction,
    StopCollectingAction,
    CliBuilder,
    from_arg_parser,
    from_full_parser,
    add_option,
    add_parameter,
    add_subcommands,
    update_parser,
    bind_parser,
    map,
    combine,
    into_parts,
    check_for_help_and_version,
]

import Arg
import Base exposing [
    ArgParser,
    ArgParserState,
    ArgParserResult,
    on_successful_arg_parse,
    map_successfully_parsed,
    ArgExtractErr,
    OptionConfig,
    help_option,
    version_option,
    ParameterConfig,
    SubcommandConfig,
]
import Parser exposing [ParsedArg]

GetOptionsAction : { get_options : {} }
GetParamsAction : { get_params : {} }
StopCollectingAction : []

CliBuilder data from_action to_action := {
    parser : ArgParser data,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : Dict Str SubcommandConfig,
}

from_arg_parser : (List ParsedArg -> Result { data : data, remaining_args : List ParsedArg } ArgExtractErr) -> CliBuilder data from_action to_action
from_arg_parser = \parser ->
    new_parser = \{ args, subcommand_path } ->
        when parser args is
            Ok { data, remaining_args } -> SuccessfullyParsed { data, remaining_args, subcommand_path }
            Err err -> IncorrectUsage err { subcommand_path }

    @CliBuilder {
        parser: new_parser,
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

from_full_parser : ArgParser data -> CliBuilder data from_action to_action
from_full_parser = \parser ->
    @CliBuilder {
        parser,
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

add_option : CliBuilder state from_action to_action, OptionConfig -> CliBuilder state from_action to_action
add_option = \@CliBuilder builder, new_option ->
    @CliBuilder { builder & options: List.append builder.options new_option }

add_parameter : CliBuilder state from_action to_action, ParameterConfig -> CliBuilder state from_action to_action
add_parameter = \@CliBuilder builder, new_parameter ->
    @CliBuilder { builder & parameters: List.append builder.parameters new_parameter }

add_subcommands : CliBuilder state from_action to_action, Dict Str SubcommandConfig -> CliBuilder state from_action to_action
add_subcommands = \@CliBuilder builder, new_subcommands ->
    @CliBuilder { builder & subcommands: Dict.insertAll builder.subcommands new_subcommands }

set_parser : CliBuilder state from_action to_action, ArgParser next_state -> CliBuilder next_state from_action to_action
set_parser = \@CliBuilder builder, parser ->
    @CliBuilder {
        options: builder.options,
        parameters: builder.parameters,
        subcommands: builder.subcommands,
        parser,
    }

update_parser : CliBuilder state from_action to_action, ({ data : state, remaining_args : List ParsedArg } -> Result { data : next_state, remaining_args : List ParsedArg } ArgExtractErr) -> CliBuilder next_state from_action to_action
update_parser = \@CliBuilder builder, updater ->
    new_parser =
        on_successful_arg_parse builder.parser \{ data, remaining_args, subcommand_path } ->
            when updater { data, remaining_args } is
                Err err -> IncorrectUsage err { subcommand_path }
                Ok { data: updated_data, remaining_args: rest_of_args } ->
                    SuccessfullyParsed { data: updated_data, remaining_args: rest_of_args, subcommand_path }

    set_parser (@CliBuilder builder) new_parser

bind_parser : CliBuilder state from_action to_action, (ArgParserState state -> ArgParserResult (ArgParserState next_state)) -> CliBuilder next_state from_action to_action
bind_parser = \@CliBuilder builder, updater ->
    new_parser : ArgParser next_state
    new_parser =
        on_successful_arg_parse builder.parser \{ data, remaining_args, subcommand_path } ->
            updater { data, remaining_args, subcommand_path }

    set_parser (@CliBuilder builder) new_parser

into_parts :
    CliBuilder state from_action to_action
    -> {
        parser : ArgParser state,
        options : List OptionConfig,
        parameters : List ParameterConfig,
        subcommands : Dict Str SubcommandConfig,
    }
into_parts = \@CliBuilder builder -> builder

map : CliBuilder a from_action to_action, (a -> b) -> CliBuilder b from_action to_action
map = \@CliBuilder builder, mapper ->
    combined_parser = \input ->
        builder.parser input
        |> map_successfully_parsed \{ data, remaining_args, subcommand_path } ->
            { data: mapper data, remaining_args, subcommand_path }

    @CliBuilder {
        parser: combined_parser,
        options: builder.options,
        parameters: builder.parameters,
        subcommands: builder.subcommands,
    }

combine : CliBuilder a action1 action2, CliBuilder b action2 action3, (a, b -> c) -> CliBuilder c action1 action3
combine = \@CliBuilder left, @CliBuilder right, combiner ->
    combined_parser = \input ->
        when left.parser input is
            ShowVersion -> ShowVersion
            ShowHelp sp -> ShowHelp sp
            IncorrectUsage arg_extract_err sp -> IncorrectUsage arg_extract_err sp
            SuccessfullyParsed { data, remaining_args, subcommand_path } ->
                when right.parser { args: remaining_args, subcommand_path } is
                    ShowVersion -> ShowVersion
                    ShowHelp sp -> ShowHelp sp
                    IncorrectUsage arg_extract_err sp -> IncorrectUsage arg_extract_err sp
                    SuccessfullyParsed { data: data2, remaining_args: rest_of_args, subcommand_path: next_sp } ->
                        SuccessfullyParsed { data: combiner data data2, remaining_args: rest_of_args, subcommand_path: next_sp }

    @CliBuilder {
        parser: combined_parser,
        options: List.concat left.options right.options,
        parameters: List.concat left.parameters right.parameters,
        subcommands: Dict.insertAll left.subcommands right.subcommands,
    }

flag_was_passed : OptionConfig, List ParsedArg -> Bool
flag_was_passed = \option, args ->
    List.any args \arg ->
        when arg is
            Short short -> short == option.short
            ShortGroup sg -> List.any sg.names \n -> n == option.short
            Long long -> long.name == option.long
            Parameter _p -> Bool.false

check_for_help_and_version : CliBuilder state from_action to_action -> CliBuilder state from_action to_action
check_for_help_and_version = \@CliBuilder builder ->
    new_parser = \{ args, subcommand_path } ->
        when builder.parser { args, subcommand_path } is
            ShowHelp sp -> ShowHelp sp
            ShowVersion -> ShowVersion
            other ->
                if flag_was_passed help_option args then
                    ShowHelp { subcommand_path }
                else if flag_was_passed version_option args then
                    ShowVersion
                else
                    other

    @CliBuilder {
        options: builder.options |> List.concat [help_option, version_option],
        parameters: builder.parameters,
        subcommands: builder.subcommands,
        parser: new_parser,
    }

expect
    { parser } =
        from_arg_parser \args -> Ok { data: Inspect.toStr args, remaining_args: [] }
        |> map Inspected
        |> into_parts

    out = parser { args: [Parameter (Arg.from_str "123")], subcommand_path: [] }

    out
    == SuccessfullyParsed {
        data: Inspected "[(Parameter \"123\")]",
        remaining_args: [],
        subcommand_path: [],
    }

expect
    args = [Parameter (Arg.from_str "-h")]

    flag_was_passed help_option args |> Bool.not

expect
    args = [Short "h"]

    flag_was_passed help_option args

expect
    args = [Long { name: "help", value: Err NoValue }]

    flag_was_passed help_option args

expect
    args = [Long { name: "help", value: Ok (Arg.from_str "123") }]

    flag_was_passed help_option args
