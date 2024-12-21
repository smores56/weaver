module [finish, optional, required, SubcommandParserConfig]

import Arg
import Base exposing [
    ArgParser,
    ArgParserState,
    ArgParserResult,
    on_successful_arg_parse,
    SubcommandConfig,
]
import Builder exposing [
    CliBuilder,
    GetOptionsAction,
    GetParamsAction,
]

SubcommandParserConfig sub_state : {
    name : Str,
    parser : ArgParser sub_state,
    config : SubcommandConfig,
}

## Bundle a CLI builder into a subcommand.
##
## Subcommands use the same CLI builder that top-level CLIs do,
## so they are composed using the same tools. The difference lies in
## how subcommands are prepared for usage by parents. In addition to
## providing a `name` and a `description`, you also provide a `mapper`,
## which is a function that converts the subcommand's data into a common
## type that all subcommands under a parent command need to share. This
## is required since the parent command will have a field (added with
## the [field] function) that must have a unified type.
##
## ```roc
## fooSubcommand =
##     { Cli.weave <-
##         foo: Opt.str { short: "f" },
##         bar: Opt.str { short: "b" },
##     }
##     |> SubCmd.finish { name: "foobar", description: "Foo and bar subcommand", mapper: FooBar }
## ```
finish : CliBuilder state fromAction toAction, { name : Str, description ? Str, mapper : state -> commonState } -> { name : Str, parser : ArgParser commonState, config : SubcommandConfig }
finish = \builder, { name, description ? "", mapper } ->
    { options, parameters, subcommands, parser } =
        builder
        |> Builder.check_for_help_and_version
        |> Builder.update_parser \{ data, remaining_args } ->
            Ok { data: mapper data, remaining_args }
        |> Builder.into_parts

    config = {
        description,
        options,
        parameters,
        subcommands: HasSubcommands subcommands,
    }

    { name, config, parser }

## Check the first parameter passed to see if a subcommand was called.
get_first_arg_to_check_for_subcommand_call :
    ArgParserState *,
    List (SubcommandParserConfig sub_state),
    (Result (SubcommandParserConfig sub_state) [NotFound] -> ArgParserResult (ArgParserState state))
    -> ArgParserResult (ArgParserState state)
get_first_arg_to_check_for_subcommand_call = \{ remaining_args, subcommand_path }, subcommand_parsers, callback ->
    find_subcommand = \param ->
        subcommand_parsers
        |> List.findFirst \sc -> Ok sc.name == (param |> Result.try Arg.to_str)

    when List.first remaining_args is
        Err ListWasEmpty -> callback (find_subcommand (Err NoValue))
        Ok first_arg ->
            when first_arg is
                Short short -> IncorrectUsage (UnrecognizedShortArg short) { subcommand_path }
                Long long -> IncorrectUsage (UnrecognizedLongArg long.name) { subcommand_path }
                ShortGroup sg -> IncorrectUsage (UnrecognizedShortArg (sg.names |> List.first |> Result.withDefault "")) { subcommand_path }
                Parameter p -> callback (find_subcommand (Ok p))

## Use previously defined subcommands as data in a parent CLI builder.
##
## Once all options have been parsed, we then check the first parameter
## passed to see if it's one of the provided subcommands. If so, we parse
## the remaining arguments as that subcommand's data, and otherwise continue
## parsing the current command.
##
## The [optional] function can only be used after all  `Opt` fields have been
## registered (if any) as we don't want to parse options for a subcommand
## instead of a parent, and cannot be used after any parameters have been
## registered. This is enforced using the type state pattern, where we encode
## the state of the program into its types. If you're curious, check the
## internal `Builder` module to see how this works using the `action` type
## variable.
##
## ```roc
## expect
##     foo_subcommand =
##         Opt.str { short: "f" }
##         |> SubCmd.finish { name: "foo", description: "Foo subcommand", mapper: Foo }
##
##     bar_subcommand =
##         Opt.str { short: "b" }
##         |> SubCmd.finish { name: "bar", description: "Bar subcommand", mapper: Bar }
##
##     { parser } =
##         SubCmd.optional [foo_subcommand, bar_subcommand],
##         |> Cli.finish { name: "example" }
##         |> Cli.assert_valid
##
##     parser ["example", "bar", "-b", "abc"]
##     == SuccessfullyParsed (Ok (Bar "abc"))
## ```
optional : List (SubcommandParserConfig sub_state) -> CliBuilder (Result sub_state [NoSubcommand]) GetOptionsAction GetParamsAction
optional = \subcommand_configs ->
    subcommands =
        subcommand_configs
        |> List.map \{ name, config } -> (name, config)
        |> Dict.fromList

    full_parser = \{ args, subcommand_path } ->
        get_first_arg_to_check_for_subcommand_call { data: {}, remaining_args: args, subcommand_path } subcommand_configs \subcommand_found ->
            when subcommand_found is
                Err NotFound ->
                    SuccessfullyParsed { data: Err NoSubcommand, remaining_args: args, subcommand_path }

                Ok subcommand ->
                    sub_parser =
                        on_successful_arg_parse subcommand.parser \{ data: sub_data, remaining_args: sub_remaining_args, subcommand_path: sub_subcommand_path } ->
                            SuccessfullyParsed { data: Ok sub_data, remaining_args: sub_remaining_args, subcommand_path: sub_subcommand_path }

                    sub_parser {
                        args: args |> List.dropFirst 1,
                        subcommand_path: subcommand_path |> List.append subcommand.name,
                    }

    Builder.from_full_parser full_parser
    |> Builder.add_subcommands subcommands

## Use previously defined subcommands as data in a parent CLI builder.
##
## Once all options have been parsed, we then check the first parameter
## passed to see if it's one of the provided subcommands. If so, we parse
## the remaining arguments as that subcommand's data, and otherwise we
## fail parsing.
##
## The [required] function can only be used after all  `Opt` fields have been
## registered (if any) as we don't want to parse options for a subcommand
## instead of a parent, and cannot be used after any parameters have been
## registered. This is enforced using the type state pattern, where we encode
## the state of the program into its types. If you're curious, check the
## internal `Builder` module to see how this works using the `action` type
## variable.
##
## ```roc
## expect
##     foo_subcommand =
##         Opt.str { short: "f" }
##         |> SubCmd.finish { name: "foo", description: "Foo subcommand", mapper: Foo }
##
##     bar_subcommand =
##         Opt.str { short: "b" }
##         |> SubCmd.finish { name: "bar", description: "Bar subcommand", mapper: Bar }
##
##     { parser } =
##         SubCmd.required [foo_subcommand, bar_subcommand],
##         |> Cli.finish { name: "example" }
##         |> Cli.assertValid
##
##     parser ["example", "bar", "-b", "abc"]
##     == SuccessfullyParsed (Bar "abc")
## ```
required : List (SubcommandParserConfig sub_data) -> CliBuilder sub_data GetOptionsAction GetParamsAction
required = \subcommand_configs ->
    subcommands =
        subcommand_configs
        |> List.map \{ name, config } -> (name, config)
        |> Dict.fromList

    full_parser = \{ args, subcommand_path } ->
        get_first_arg_to_check_for_subcommand_call { data: {}, remaining_args: args, subcommand_path } subcommand_configs \subcommand_found ->
            when subcommand_found is
                Err NotFound ->
                    IncorrectUsage NoSubcommandCalled { subcommand_path }

                Ok subcommand ->
                    sub_parser =
                        on_successful_arg_parse subcommand.parser \{ data: sub_data, remaining_args: sub_remaining_args, subcommand_path: sub_subcommand_path } ->
                            SuccessfullyParsed { data: sub_data, remaining_args: sub_remaining_args, subcommand_path: sub_subcommand_path }

                    sub_parser {
                        args: args |> List.dropFirst 1,
                        subcommand_path: subcommand_path |> List.append subcommand.name,
                    }

    Builder.from_full_parser full_parser
    |> Builder.add_subcommands subcommands
