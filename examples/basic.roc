app "basic"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        weaver: "../package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Arg,
        pf.Task.{ Task },
        weaver.Builder.{
            cliBuilder,
            finishCli,
            assertCliIsValid,
            parseOrDisplayMessage,
            finishSubcommand,
            subcommandField,
            numOption,
            maybeNumOption,
            strParam,
            maybeStrParam,
            strListParam,
            flagOption,
        },
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await

    textToDisplay =
        when parseOrDisplayMessage cliParser args is
            Ok data -> "Successfully parsed! Here's what I got:\n\n$(Inspect.toStr data)"
            Err message -> message

    Stdout.line textToDisplay

cliParser =
    subSubcommandParser1 =
        cliBuilder {
            a: <- numOption { name: Short "a", help: "An example short flag for a sub-subcommand." },
            b: <- numOption { name: Short "b", help: "Another example short flag for a sub-subcommand." },
        }
        |> finishSubcommand { name: "ss1", description: "A sub-subcommand.", mapper: SS1 }

    subSubcommandParser2 =
        cliBuilder {
            a: <- numOption { name: Short "a" },
            c: <- numOption { name: Both "c" "create" },
            data: <- strParam { name: "data", help: "Data to manipulate." },
        }
        |> finishSubcommand { name: "ss2", description: "Another sub-subcommand.", mapper: SS2 }

    subcommandParser1 =
        cliBuilder {
            d: <- maybeNumOption { name: Short "d", help: "A non-overlapping subcommand flag with s2." },
            volume: <- maybeNumOption { name: Both "v" "volume", help: "How loud to grind the gears." },
            sc: <- subcommandField [subSubcommandParser1, subSubcommandParser2],
        }
        |> finishSubcommand { name: "s1", description: "A first subcommand.", mapper: S1 }

    subcommandParser2 =
        cliBuilder {
            d: <- maybeNumOption { name: Short "d", help: "This doesn't overlap with s1's -d flag." },
        }
        |> finishSubcommand {
            name: "s2",
            description: "Another subcommand.",
            mapper: S2,
        }

    cliBuilder {
        force: <- flagOption { name: Short "f", help: "Force the task to complete." },
        sc: <- subcommandField [subcommandParser1, subcommandParser2],
        file: <- maybeStrParam { name: "file" },
        files: <- strListParam { name: "files" },
    }
    |> finishCli {
        name: "basic",
        version: "v0.0.1",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    }
    |> assertCliIsValid
