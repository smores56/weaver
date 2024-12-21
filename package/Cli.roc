## Weave together a CLI parser using the `<-` builder notation!
##
## This module is the entry point for creating CLIs using Weaver.
## To get started, call the [weave] method and pass a
## [record builder](https://www.roc-lang.org/examples/RecordBuilder/README.html)
## to it. You can pass `Opt`s, `Param`s, or `SubCmd`s as fields,
## and Weaver will automatically register them in its config as
## well as build a parser with the inferred types of the fields
## you set.
##
## ```roc
## { Cli.weave <-
##     alpha: Opt.u64 { short: "a", help: "Set the alpha level" },
##     verbosity: Opt.count { short: "v", long: "verbose", help: "How loud we should be." },
##     files: Param.str_list { name: "files", help: "The files to process." },
## }
## |> Cli.finish {
##     name: "example",
##     version: "v1.0.0",
##     authors: ["Some One <some.one@mail.com>"],
##     description: "Do some work with some files."
## }
## |> Cli.assert_valid
## ```
##
## You can also add create subcommands in the same way:
##
## ```roc
## fooSubcommand =
##     Opt.u64 { short: "a", help: "Set the alpha level" }
##     |> SubCmd.finish {
##         name: "foo",
##         description: "Foo some stuff."
##         mapper: Foo,
##     }
##
## barSubcommand =
##     # We allow two subcommands of the same parent to have overlapping
##     # fields since only one can ever be parsed at a time.
##     Opt.u64 { short: "a", help: "Set the alpha level" }
##     |> SubCmd.finish {
##         name: "bar",
##         description: "Bar some stuff."
##         mapper: Bar,
##     }
##
## { Cli.weave <-
##     verbosity: Opt.count { short: "v", long: "verbose" },
##     sc: SubCmd.optional [fooSubcommand, barSubcommand],
## }
## ```
##
## And those subcommands can have their own subcommands! But anyway...
##
## Once you have a command with all of its fields configured, you can
## turn it into a parser using the [finish] function, followed by
## the [assert_valid] function that asserts that the CLI is well configured.
##
## From there, you can take in command line arguments and use your
## data if it parses correctly:
##
## ```roc
## cliParser =
##     { Cli.weave <-
##         alpha: Opt.u64 { short: "a", help: "Set the alpha level" },
##         verbosity: Opt.count { short: "v", long: "verbose", help: "How loud we should be." },
##         files: Param.str_list { name: "files", help: "The files to process." },
##     }
##     |> Cli.finish {
##         name: "example",
##         version: "v1.0.0",
##         authors: ["Some One <some.one@mail.com>"],
##         description: "Do some work with some files."
##     }
##     |> Cli.assert_valid
##
## expect
##     cliParser
##     |> Cli.parse_or_display_message ["example", "-a", "123", "-vvv", "file.txt", "file-2.txt"] Arg.to_os_raw
##     == Ok { alpha: 123, verbosity: 3, files: ["file.txt", "file-2.txt"] }
## ```
##
## You're ready to start parsing arguments using Weaver!
##
## If you want to see more examples, check the [examples](https://github.com/smores56/weaver/tree/main/examples)
## folder in the [repository](https://github.com/smores56/weaver).
##
## _note: `Opt`s must be set before an optional `SubCmd` field is given,_
## _and the `SubCmd` field needs to be set before `Param`s are set._
## _`Param` lists also cannot be followed by anything else including_
## _themselves. These requirements ensure we parse arguments in the_
## _right order. Luckily, all of this is ensured at the type level._
module [
    CliParser,
    map,
    weave,
    finish,
    finish_without_validating,
    assert_valid,
    parse_or_display_message,
]

import Opt
import Param
import Base exposing [
    TextStyle,
    ArgParserResult,
    ArgExtractErr,
    CliConfig,
    CliConfigParams,
    map_successfully_parsed,
]
import Arg exposing [Arg]
import Parser exposing [ParsedArg, parse_args]
import Builder exposing [CliBuilder]
import Validate exposing [validate_cli, CliValidationErr]
import ErrorFormatter exposing [
    format_arg_extract_err,
    format_cli_validation_err,
]
import Help exposing [help_text, usage_help]

## A parser that interprets command line arguments and returns well-formed data.
CliParser state : {
    config : CliConfig,
    parser : List Arg -> ArgParserResult state,
    text_style : TextStyle,
}

## Map over the parsed value of a Weaver field.
##
## Useful for naming bare fields, or handling default values.
##
## ```roc
## expect
##     { parser } =
##         { Cli.weave <-
##             verbosity: Opt.count { short: "v", long: "verbose" }
##                 |> Cli.map Verbosity,
##             file: Param.maybe_str { name: "file" }
##                 |> Cli.map \f -> Result.withDefault f "NO_FILE",
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assert_valid
##
##    parser ["example", "-vvv"]
##    == SuccessfullyParsed { verbosity: Verbosity 3, file: "NO_FILE" }
## ```
map : CliBuilder a from_action to_action, (a -> b) -> CliBuilder b from_action to_action
map = \builder, mapper ->
    Builder.map builder mapper

## Begin weaving together a CLI builder using the `<-` builder notation.
##
## Check the module-level documentation for general usage instructions.
##
## ```roc
## expect
##     { parser } =
##         { Cli.weave <-
##             verbosity: Opt.count { short: "v", long: "verbose" },
##             file: Param.str { name: "file" },
##         }
##         |> Cli.finish { name: "example" }
##         |> Cli.assert_valid
##
##    parser ["example", "file.txt", "-vvv"]
##    == SuccessfullyParsed { verbosity: 3, file: "file.txt" }
## ```
weave : CliBuilder a action1 action2, CliBuilder b action2 action3, (a, b -> c) -> CliBuilder c action1 action3
weave = \left, right, combiner ->
    Builder.combine left right combiner

## Fail the parsing process if any arguments are left over after parsing.
ensure_all_args_were_parsed : List ParsedArg -> Result {} ArgExtractErr
ensure_all_args_were_parsed = \remaining_args ->
    when remaining_args is
        [] -> Ok {}
        [first, ..] ->
            extra_arg_err =
                when first is
                    Parameter param -> ExtraParamProvided param
                    Long long -> UnrecognizedLongArg long.name
                    Short short -> UnrecognizedShortArg short
                    ShortGroup sg ->
                        first_short_arg = List.first sg.names |> Result.withDefault ""
                        UnrecognizedShortArg first_short_arg

            Err extra_arg_err

## Bundle a CLI builder into a parser, ensuring that its configuration is valid.
##
## Though the majority of the validation we'd need to do for type safety is
## rendered unnecessary by the design of this library, there are some things
## that the type system isn't able to prevent. Here are the checks we currently
## perform after building your CLI parser:
##
## - All commands and subcommands must have kebab-case names.
## - All options must have either:
##   - A short flag which is a single character.
##   - A long flag which is more than one character and kebab-case.
##   - Both a short and a long flag with the above requirements.
## - All parameters must be have kebab-case names.
## - All custom option/parameter types are have kebab-case names.
## - No options can overlap, even between different subcommands, so long
##   as the options between the subcommands are ambiguous.
##   - For example, a CLI with a `-t` option at the root level and also
##     a `-t` option in the subcommand `sub` would fail validation since
##     we wouldn't know who should get the `-t` option.
##   - However, a CLI with two subcommands that each have a `-t` option
##     would not fail validation since only one subcommand can be called
##     at once.
##
## If you would like to avoid these validations, you can use [finish_without_validating]
## instead, but you may receive some suprising results when parsing because
## our parsing logic assumes the above validations have been made.
##
## ```roc
## expect
##     { Cli.weave <-
##         verbosity: Opt.count { short: "v", long: "verbose" },
##         file: Param.str { name: "file" },
##     }
##     |> Cli.finish { name: "example" }
##     |> Result.isOk
##
## expect
##     { Cli.weave <-
##         verbosity: Opt.count { short: "" },
##         file: Param.str { name: "" },
##     }
##     |> Cli.finish { name: "example" }
##     |> Result.isErr
## ```
finish : CliBuilder data from_action to_action, CliConfigParams -> Result (CliParser data) CliValidationErr
finish = \builder, params ->
    { parser, config, text_style } = finish_without_validating builder params
    try validate_cli config

    Ok { parser, config, text_style }

## Bundle a CLI builder into a parser without validating its configuration.
##
## We recommend using the [finish] function to validate your parser as our
## library's logic assumes said validation has taken place. However, this method
## could be useful if you know better than our validations about the correctness
## of your CLI.
##
## ```roc
## expect
##     { parser } =
##         { Cli.weave <-
##             verbosity: Opt.count { short: "v", long: "verbose" },
##             file: Param.maybe_str { name: "file" },
##         }
##         |> Cli.finish_without_validating { name: "example" }
##
##     parser ["example", "-v", "-v"]
##     == SuccessfullyParsed { verbosity: 2, file: Err NoValue }
## ```
finish_without_validating : CliBuilder data from_action to_action, CliConfigParams -> CliParser data
finish_without_validating = \builder, { name, authors ? [], version ? "", description ? "", text_style ? Color } ->
    { options, parameters, subcommands, parser } =
        builder
        |> Builder.check_for_help_and_version
        |> Builder.update_parser \data ->
            try ensure_all_args_were_parsed data.remaining_args

            Ok data
        |> Builder.into_parts

    config = {
        name,
        authors,
        version,
        description,
        options,
        parameters,
        subcommands: HasSubcommands subcommands,
    }

    {
        config,
        text_style,
        parser: \args ->
            parser { args: parse_args args, subcommand_path: [name] }
            |> map_successfully_parsed \{ data } -> data,
    }

## Assert that a CLI is properly configured, crashing your program if not.
##
## Given that there are some aspects of a CLI that we cannot ensure are
## correct at compile time, the easiest way to ensure that your CLI is properly
## configured is to validate it and crash immediately on failure, following the
## Fail Fast principle.
##
## You can avoid making this assertion by handling the error yourself or
## by finish your CLI with the [finish_without_validating] function, but
## the validations we perform (detailed in [finish]'s docs) are important
## for correct parsing.
##
## ```roc
## Opt.num { short: "a" }
## |> Cli.finish { name: "example" }
## |> Cli.assert_valid
## ```
assert_valid : Result (CliParser data) CliValidationErr -> CliParser data
assert_valid = \result ->
    when result is
        Ok cli -> cli
        Err err -> crash (format_cli_validation_err err)

## Parse arguments using a CLI parser or show a useful message on failure.
##
## We have the following priorities in returning messages to the user:
## 1) If the `-h/--help` flag is passed, the help page for the command/subcommand
##    called will be displayed no matter if your arguments were correctly parsed.
## 2) If the `-V/--version` flag is passed, the version for the app will
##    be displayed no matter if your arguments were correctly parsed.
## 3) If the provided arguments were parsed and neither of the above two
##    built-in flags were passed, we return to you your data.
## 4) If the provided arguments were not correct, we return a short message
##    with which argument was not provided correctly, followed by the
##    usage section of the relevant command/subcommand's help text.
##
## ```roc
## exampleCli =
##     { Cli.weave <-
##         verbosity: Opt.count { short: "v", long: "verbose" },
##         alpha: Opt.maybe_num { short: "a", long: "alpha" },
##     }
##     |> Cli.finish {
##         name: "example",
##         version: "v0.1.0",
##         description: "An example CLI.",
##     }
##     |> Cli.assert_valid
##
## expect
##     exampleCli
##     |> Cli.parse_or_display_message ["example", "-h"] Arg.to_os_raw
##     == Err
##         """
##         example v0.1.0
##
##         An example CLI.
##
##         Usage:
##           example [OPTIONS]
##
##         Options:
##           -v             How verbose our logs should be.
##           -a, --alpha    Set the alpha level.
##           -h, --help     Show this help page.
##           -V, --version  Show the version.
##         """
##
## expect
##     exampleCli
##     |> Cli.parse_or_display_message ["example", "-V"] Arg.to_os_raw
##     == Err "v0.1.0"
##
## expect
##     exampleCli
##     |> Cli.parse_or_display_message ["example", "-v"] Arg.to_os_raw
##     == Ok { verbosity: 1 }
##
## expect
##     exampleCli
##     |> Cli.parse_or_display_message ["example", "-x"] Arg.to_os_raw
##     == Err
##         """
##         Error: The argument -x was not recognized.
##
##         Usage:
##           example [OPTIONS]
##         """
## ```
parse_or_display_message : CliParser data, List arg, (arg -> [Unix (List U8), Windows (List U16)]) -> Result data Str
parse_or_display_message = \parser, external_args, to_raw_arg ->
    args =
        external_args
        |> List.map to_raw_arg
        |> List.map Arg.from_raw_arg

    when parser.parser args is
        SuccessfullyParsed data -> Ok data
        ShowHelp { subcommand_path } -> Err (help_text parser.config subcommand_path parser.text_style)
        ShowVersion -> Err parser.config.version
        IncorrectUsage err { subcommand_path } ->
            usage_str = usage_help parser.config subcommand_path parser.text_style
            incorrect_usage_str =
                """
                Error: $(format_arg_extract_err err)

                $(usage_str)
                """

            Err incorrect_usage_str

expect
    Opt.count { short: "v" }
    |> Cli.map Verbosity
    |> Cli.finish { name: "empty" }
    |> Result.isOk

expect
    Opt.count { short: "" }
    |> Cli.map Verbosity
    |> Cli.finish { name: "example" }
    |> Result.isErr

expect
    { Cli.weave <-
        verbosity: Opt.count { short: "v" },
        points: Param.str { name: "points" },
    }
    |> Cli.finish { name: "test" }
    |> Result.isOk
