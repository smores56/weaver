module [
    GetOptionsAction,
    GetParamsAction,
    StopCollectingAction,
    CliBuilder,
    fromArgParser,
    fromFullParser,
    addOption,
    addParameter,
    addSubcommands,
    updateParser,
    bindParser,
    map,
    combine,
    intoParts,
    checkForHelpAndVersion,
]

import Base exposing [
    ArgParser,
    ArgParserState,
    ArgParserResult,
    onSuccessfulArgParse,
    mapSuccessfullyParsed,
    ArgExtractErr,
    OptionConfig,
    helpOption,
    versionOption,
    ParameterConfig,
    SubcommandConfig,
]
import Parser exposing [Arg]

GetOptionsAction : { getOptions : {} }
GetParamsAction : { getParams : {} }
StopCollectingAction : []

CliBuilder data from_action to_action := {
    parser : ArgParser data,
    options : List OptionConfig,
    parameters : List ParameterConfig,
    subcommands : Dict Str SubcommandConfig,
}

from_arg_parser : (List Arg -> Result { data : data, remainingArgs : List Arg } ArgExtractErr) -> CliBuilder data fromAction toAction
from_arg_parser = \parser ->
    new_parser = \{ args, subcommand_path } ->
        when parser args is
            Ok { data, remaining_args } -> SuccessfullyParsed { data, remainingArgs, subcommandPath }
            Err err -> IncorrectUsage err { subcommandPath }

    @CliBuilder {
        parser: newParser,
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

from_full_parser : ArgParser data -> CliBuilder data fromAction toAction
from_full_parser = \parser ->
    @CliBuilder {
        parser,
        options: [],
        parameters: [],
        subcommands: Dict.empty {},
    }

add_option : CliBuilder state fromAction toAction, OptionConfig -> CliBuilder state fromAction toAction
add_option = \@CliBuilder builder, new_option ->
    @CliBuilder { builder & options: List.append builder.options newOption }

add_parameter : CliBuilder state fromAction toAction, ParameterConfig -> CliBuilder state fromAction toAction
add_parameter = \@CliBuilder builder, new_parameter ->
    @CliBuilder { builder & parameters: List.append builder.parameters newParameter }

add_subcommands : CliBuilder state fromAction toAction, Dict Str SubcommandConfig -> CliBuilder state fromAction toAction
add_subcommands = \@CliBuilder builder, new_subcommands ->
    @CliBuilder { builder & subcommands: Dict.insertAll builder.subcommands newSubcommands }

set_parser : CliBuilder state fromAction toAction, ArgParser nextState -> CliBuilder nextState fromAction toAction
set_parser = \@CliBuilder builder, parser ->
    @CliBuilder {
        options: builder.options,
        parameters: builder.parameters,
        subcommands: builder.subcommands,
        parser,
    }

update_parser : CliBuilder state fromAction toAction, ({ data : state, remainingArgs : List Arg } -> Result { data : nextState, remainingArgs : List Arg } ArgExtractErr) -> CliBuilder nextState fromAction toAction
update_parser = \@CliBuilder builder, updater ->
    new_parser =
        onSuccessfulArgParse builder.parser \{ data, remaining_args, subcommand_path } ->
            when updater { data, remainingArgs } is
                Err err -> IncorrectUsage err { subcommandPath }
                Ok { data: updated_data, remaining_args: rest_of_args } ->
                    SuccessfullyParsed { data: updatedData, remainingArgs: restOfArgs, subcommandPath }

    setParser (@CliBuilder builder) newParser

bind_parser : CliBuilder state fromAction toAction, (ArgParserState state -> ArgParserResult (ArgParserState nextState)) -> CliBuilder nextState fromAction toAction
bind_parser = \@CliBuilder builder, updater ->
    new_parser : ArgParser nextState
    new_parser =
        onSuccessfulArgParse builder.parser \{ data, remaining_args, subcommand_path } ->
            updater { data, remainingArgs, subcommandPath }

    setParser (@CliBuilder builder) newParser

into_parts :
    CliBuilder state fromAction toAction
    -> {
        parser : ArgParser state,
        options : List OptionConfig,
        parameters : List ParameterConfig,
        subcommands : Dict Str SubcommandConfig,
    }
into_parts = \@CliBuilder builder -> builder

map : CliBuilder a fromAction toAction, (a -> b) -> CliBuilder b fromAction toAction
map = \@CliBuilder builder, mapper ->
    combined_parser = \input ->
        builder.parser input
        |> mapSuccessfullyParsed \{ data, remaining_args, subcommand_path } ->
            { data: mapper data, remainingArgs, subcommandPath }

    @CliBuilder {
        parser: combinedParser,
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
            IncorrectUsage arg_extract_err sp -> IncorrectUsage argExtractErr sp
            SuccessfullyParsed { data, remaining_args, subcommand_path } ->
                when right.parser { args: remainingArgs, subcommandPath } is
                    ShowVersion -> ShowVersion
                    ShowHelp sp -> ShowHelp sp
                    IncorrectUsage arg_extract_err sp -> IncorrectUsage argExtractErr sp
                    SuccessfullyParsed { data: data2, remaining_args: rest_of_args, subcommand_path: next_sp } ->
                        SuccessfullyParsed { data: combiner data data2, remainingArgs: restOfArgs, subcommandPath: nextSp }

    @CliBuilder {
        parser: combinedParser,
        options: List.concat left.options right.options,
        parameters: List.concat left.parameters right.parameters,
        subcommands: Dict.insertAll left.subcommands right.subcommands,
    }

flag_was_passed : OptionConfig, List Arg -> Bool
flag_was_passed = \option, args ->
    List.any args \arg ->
        when arg is
            Short short -> short == option.short
            ShortGroup sg -> List.any sg.names \n -> n == option.short
            Long long -> long.name == option.long
            Parameter _p -> Bool.false

check_for_help_and_version : CliBuilder state fromAction toAction -> CliBuilder state fromAction toAction
check_for_help_and_version = \@CliBuilder builder ->
    new_parser = \{ args, subcommand_path } ->
        when builder.parser { args, subcommandPath } is
            ShowHelp sp -> ShowHelp sp
            ShowVersion -> ShowVersion
            other ->
                if flagWasPassed helpOption args then
                    ShowHelp { subcommandPath }
                else if flagWasPassed versionOption args then
                    ShowVersion
                else
                    other

    @CliBuilder {
        options: builder.options |> List.concat [helpOption, versionOption],
        parameters: builder.parameters,
        subcommands: builder.subcommands,
        parser: newParser,
    }

expect
    { parser } =
        fromArgParser \args -> Ok { data: Inspect.toStr args, remainingArgs: [] }
        |> map Inspected
        |> intoParts

    out = parser { args: [Parameter "123"], subcommandPath: [] }

    out
    == SuccessfullyParsed {
        data: Inspected "[(Parameter \"123\")]",
        remainingArgs: [],
        subcommandPath: [],
    }

expect
    args = [Parameter "-h"]

    flagWasPassed helpOption args |> Bool.not

expect
    args = [Short "h"]

    flagWasPassed helpOption args

expect
    args = [Long { name: "help", value: Err NoValue }]

    flagWasPassed helpOption args

expect
    args = [Long { name: "help", value: Ok "123" }]

    flagWasPassed helpOption args
