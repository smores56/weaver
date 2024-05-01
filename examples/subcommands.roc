app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
    weaver: "../package/main.roc",
}

import pf.Stdout
import pf.Arg
import pf.Task exposing [Task]
import weaver.Opt
import weaver.Cli
import weaver.Param
import weaver.Subcommand

main =
    args = Arg.list!

    when Cli.parseOrDisplayMessage cliParser args is
        Ok data ->
            Stdout.line! "Successfully parsed! Here's what I got:"
            Stdout.line! ""
            Stdout.line! (Inspect.toStr data)

        Err message ->
            Stdout.line! message

            Task.err (Exit 1 "")

cliParser =
    Cli.weave {
        force: <- Opt.flag { short: "f", help: "Force the task to complete." },
        sc: <- Subcommand.field [subcommandParser1, subcommandParser2],
        file: <- Param.maybeStr { name: "file", help: "The file to process." },
        files: <- Param.strList { name: "files", help: "The rest of the files." },
    }
    |> Cli.finish {
        name: "subcommands",
        version: "v0.0.1",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    }
    |> Cli.assertValid

subcommandParser1 =
    Cli.weave {
        d: <- Opt.maybeU64 { short: "d", help: "A non-overlapping subcommand flag with s2." },
        volume: <- Opt.maybeU64 { short: "v", long: "volume", help: "How loud to grind the gears." },
        sc: <- Subcommand.field [subSubcommandParser1, subSubcommandParser2],
    }
    |> Subcommand.finish { name: "s1", description: "A first subcommand.", mapper: S1 }

subcommandParser2 =
    Cli.weave {
        d: <- Opt.maybeU64 { short: "d", help: "This doesn't overlap with s1's -d flag." },
    }
    |> Subcommand.finish {
        name: "s2",
        description: "Another subcommand.",
        mapper: S2,
    }

subSubcommandParser1 =
    Cli.weave {
        a: <- Opt.u64 { short: "a", help: "An example short flag for a sub-subcommand." },
        b: <- Opt.u64 { short: "b", help: "Another example short flag for a sub-subcommand." },
    }
    |> Subcommand.finish { name: "ss1", description: "A sub-subcommand.", mapper: SS1 }

subSubcommandParser2 =
    Cli.weave {
        a: <- Opt.u64 { short: "a", help: "Set the alpha level." },
        c: <- Opt.u64 { short: "c", long: "create", help: "Create a doohickey." },
        data: <- Param.str { name: "data", help: "Data to manipulate." },
    }
    |> Subcommand.finish { name: "ss2", description: "Another sub-subcommand.", mapper: SS2 }
