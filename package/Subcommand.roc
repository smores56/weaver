module [finish, field]

import Base exposing [
    ArgParser,
    ArgParserState,
    ArgParserResult,
    onSuccessfulArgParse,
    SubcommandConfig,
]
import Builder exposing [
    CliBuilder,
    GetOptionsAction,
    GetParamsAction,
]

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
##     Cli.weave {
##         foo: <- Opt.str { short: "f" },
##     }
##     |> Subcommand.finish { name: "foo", description: "Foo subcommand", mapper: Foo }
## ```
finish : CliBuilder state action, { name : Str, description ? Str, mapper : state -> commonState } -> { name : Str, parser : ArgParser commonState, config : SubcommandConfig }
finish = \builder, { name, description ? "", mapper } ->
    { options, parameters, subcommands, parser } =
        builder
        |> Builder.checkForHelpAndVersion
        |> Builder.updateParser \{ data, remainingArgs } ->
            Ok { data: mapper data, remainingArgs }
        |> Builder.intoParts

    config = {
        description,
        options,
        parameters,
        subcommands: HasSubcommands subcommands,
    }

    { name, config, parser }

## Check the first parameter passed to see if a subcommand was called.
getFirstArgToCheckForSubcommandCall : ArgParserState (Result state [NoSubcommand] -> subState), (Str -> ArgParserResult (ArgParserState subState)) -> ArgParserResult (ArgParserState subState)
getFirstArgToCheckForSubcommandCall = \{ data, remainingArgs, subcommandPath }, callback ->
    when List.first remainingArgs is
        Err ListWasEmpty -> SuccessfullyParsed { data: data (Err NoSubcommand), remainingArgs, subcommandPath }
        Ok firstArg ->
            when firstArg is
                Short short -> IncorrectUsage (UnrecognizedShortArg short) { subcommandPath }
                Long long -> IncorrectUsage (UnrecognizedLongArg long.name) { subcommandPath }
                ShortGroup sg -> IncorrectUsage (UnrecognizedShortArg (sg.names |> List.first |> Result.withDefault "")) { subcommandPath }
                Parameter p -> callback p

## Use previously defined subcommands as data in a parent CLI builder.
##
## Once all options have been parsed, we then check the first parameter
## passed to see if it's one of the provided subcommands. If so, we parse
## the remaining arguments as that subcommand's data, and otherwise continue
## parsing the current command.
##
## The [field] function can only be used after all  `Opt` fields have been
## registered (if any) as we don't want to parse options for a subcommand
## instead of a parent, and cannot be used after any parameters have been
## registered. This is enforced using the type state pattern, where we encode
## the state of the program into its types. If you're curious, check the
## internal `Builder` module to see how this works using the `action` type
## variable.
##
## ```roc
## expect
##     fooSubcommand =
##         Cli.weave {
##             foo: <- Opt.str { short: "f" },
##         }
##         |> Subcommand.finish { name: "foo", description: "Foo subcommand", mapper: Foo }
##
##     barSubcommand =
##         Cli.weave {
##             bar: <- Opt.str { short: "b" },
##         }
##         |> Subcommand.finish { name: "bar", description: "Bar subcommand", mapper: Bar }
##
##     Cli.weave {
##         sc: <- Subcommand.field [fooSubcommand, barSubcommand],
##     }
##     |> Cli.finish { name: "example" }
##     |> Cli.assertValid
##     |> Cli.parseOrDisplayMessage ["example", "bar", "-b", "abc"]
##     == Ok { sc: Ok (Bar { b: "abc" }) }
## ```
field :
    List {
        name : Str,
        parser : ArgParser subState,
        config : SubcommandConfig,
    }
    -> (CliBuilder (Result subState [NoSubcommand] -> state) GetOptionsAction -> CliBuilder state GetParamsAction)
field = \subcommandConfigs ->
    subcommands =
        subcommandConfigs
        |> List.map \{ name, config } -> (name, config)
        |> Dict.fromList

    \builder ->
        builder
        |> Builder.addSubcommands subcommands
        |> Builder.bindParser \{ data, remainingArgs, subcommandPath } ->
            firstParam <- getFirstArgToCheckForSubcommandCall { data, remainingArgs, subcommandPath }
            subcommandFound =
                subcommandConfigs
                |> List.findFirst \subConfig -> subConfig.name == firstParam

            when subcommandFound is
                Err NotFound ->
                    SuccessfullyParsed { data: data (Err NoSubcommand), remainingArgs, subcommandPath }

                Ok subcommand ->
                    subParser =
                        { data: subData, remainingArgs: subRemainingArgs, subcommandPath: subSubcommandPath } <- onSuccessfulArgParse subcommand.parser
                        SuccessfullyParsed { data: data (Ok subData), remainingArgs: subRemainingArgs, subcommandPath: subSubcommandPath }

                    subParser {
                        args: List.dropFirst remainingArgs 1,
                        subcommandPath: subcommandPath |> List.append firstParam,
                    }
