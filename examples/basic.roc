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
            finishSubcommand,
            subcommandField,
            numOption,
            strParam,
        },
        weaver.Help.{ helpText },
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await
    { parser, config } = cliParser

    textToDisplay =
        when parser args is
            Ok data -> "Successfully parsed! Here's what I got:\n\n$(Inspect.toStr data)"
            Err err -> "Error while extracting args: $(Inspect.toStr err)\n\n$(helpText { config })"

    Stdout.line textToDisplay

cliParser =
    subSubcommandParser1 =
        cliBuilder {
            a: <- numOption { name: Short "a" },
            b: <- numOption { name: Short "b" },
        }
        |> finishSubcommand { name: "ss1", description: "", mapper: SS1 }

    subSubcommandParser2 =
        cliBuilder {
            a: <- numOption { name: Short "a" },
            c: <- numOption { name: Short "c" },
        }
        |> finishSubcommand { name: "ss2", description: "", mapper: SS2 }

    subcommandParser1 =
        cliBuilder {
            d: <- numOption { name: Short "d" },
            e: <- numOption { name: Short "e" },
            sc: <- subcommandField [subSubcommandParser1, subSubcommandParser2],
        }
        |> finishSubcommand { name: "s1", description: "", mapper: S1 }

    subcommandParser2 =
        cliBuilder {
            d: <- numOption { name: Short "d" },
            f: <- numOption { name: Short "f" },
        }
        |> finishSubcommand { name: "s2", description: "", mapper: S2 }

    cliBuilder {
        x: <- numOption { name: Short "x" },
        sc: <- subcommandField [subcommandParser1, subcommandParser2],
        y: <- strParam { name: "y" },
    }
    |> finishCli { name: "basic", version: "v0.0.1", authors: ["John Doe <john.doe@mail.com>"] }
    |> assertCliIsValid
