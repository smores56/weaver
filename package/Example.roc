interface Example
    exposes [cliParser]
    imports [Arg, Cli, Param, Subcommand]

cliParser =
    subSubcommandParser1 =
        Cli.weave {
            a: <- Arg.num { short: "a", help: "An example short flag for a sub-subcommand." },
            b: <- Arg.num { short: "b", help: "Another example short flag for a sub-subcommand." },
        }
        |> Subcommand.finish { name: "ss1", description: "A sub-subcommand.", mapper: SS1 }

    subSubcommandParser2 =
        Cli.weave {
            a: <- Arg.num { short: "a", help: "Set the alpha level." },
            c: <- Arg.num { short: "c", long: "create", help: "Create a doohickey." },
            data: <- Param.str { name: "data", help: "Data to manipulate." },
        }
        |> Subcommand.finish { name: "ss2", description: "Another sub-subcommand.", mapper: SS2 }

    subcommandParser1 =
        Cli.weave {
            d: <- Arg.maybeNum { short: "d", help: "A non-overlapping subcommand flag with s2." },
            volume: <- Arg.maybeNum { short: "v", long: "volume", help: "How loud to grind the gears." },
            sc: <- Subcommand.field [subSubcommandParser1, subSubcommandParser2],
        }
        |> Subcommand.finish { name: "s1", description: "A first subcommand.", mapper: S1 }

    subcommandParser2 =
        Cli.weave {
            d: <- Arg.maybeNum { short: "d", help: "This doesn't overlap with s1's -d flag." },
        }
        |> Subcommand.finish {
            name: "s2",
            description: "Another subcommand.",
            mapper: S2,
        }

    Cli.weave {
        force: <- Arg.flag { short: "f", help: "Force the task to complete." },
        sc: <- Subcommand.field [subcommandParser1, subcommandParser2],
        file: <- Param.maybeStr { name: "file" },
        files: <- Param.strList { name: "files" },
    }
    |> Cli.finish {
        name: "basic",
        version: "v0.0.1",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    }
    |> Cli.assertValid
