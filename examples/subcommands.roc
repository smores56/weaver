app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    weaver: "../package/main.roc",
}

import pf.Arg
import pf.Stdout
import weaver.Opt
import weaver.Cli
import weaver.Param
import weaver.SubCmd

main! = \{} ->
    args = Arg.list! {}

    data =
        Cli.parse_or_display_message cli_parser args
        |> try Result.onErr! \message ->
            try Stdout.line! message
            Err (Exit 1 "")

    try Stdout.line! "Successfully parsed! Here's what I got:"
    try Stdout.line! ""
    try Stdout.line! (Inspect.toStr data)

    Ok {}

cli_parser =
    { Cli.weave <-
        force: Opt.flag { short: "f", help: "Force the task to complete." },
        sc: SubCmd.optional [subcommand_parser1, subcommand_parser2],
        file: Param.maybe_str { name: "file", help: "The file to process." },
        files: Param.str_list { name: "files", help: "The rest of the files." },
    }
    |> Cli.finish {
        name: "subcommands",
        version: "v0.0.1",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    }
    |> Cli.assert_valid

subcommand_parser1 =
    { Cli.weave <-
        d: Opt.maybe_u64 { short: "d", help: "A non-overlapping subcommand flag with s2." },
        volume: Opt.maybe_u64 { short: "v", long: "volume", help: "How loud to grind the gears." },
        sc: SubCmd.optional [sub_subcommand_parser1, sub_subcommand_parser2],
    }
    |> SubCmd.finish { name: "s1", description: "A first subcommand.", mapper: S1 }

subcommand_parser2 =
    Opt.maybe_u64 { short: "d", help: "This doesn't overlap with s1's -d flag." }
    |> Cli.map DFlag
    |> SubCmd.finish {
        name: "s2",
        description: "Another subcommand.",
        mapper: S2,
    }

sub_subcommand_parser1 =
    { Cli.weave <-
        a: Opt.u64 { short: "a", help: "An example short flag for a sub-subcommand." },
        b: Opt.u64 { short: "b", help: "Another example short flag for a sub-subcommand." },
    }
    |> SubCmd.finish { name: "ss1", description: "A sub-subcommand.", mapper: SS1 }

sub_subcommand_parser2 =
    { Cli.weave <-
        a: Opt.u64 { short: "a", help: "Set the alpha level." },
        c: Opt.u64 { short: "c", long: "create", help: "Create a doohickey." },
        data: Param.str { name: "data", help: "Data to manipulate." },
    }
    |> SubCmd.finish { name: "ss2", description: "Another sub-subcommand.", mapper: SS2 }
