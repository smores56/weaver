app "basic"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        roclap: "../package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Arg,
        pf.Task.{ Task },
        roclap.Builder.{
            cliBuilder,
            cliBuilderWithSubcommands,
            finishCli,
            finishSubcommand,
            subcommandField,
            numOption,
            strParam,
        },
        roclap.Help.{ helpText },
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await
    { parser, config } = cliParser

    when parser args is
        Ok data ->
            data
            |> Inspect.toStr
            |> Stdout.line

        Err err ->
            _ <- Stdout.line ("Error: $(Inspect.toStr err)")
                |> Task.await
            Stdout.line (helpText config)

cliParser =
    subSubcommandParser1 =
        cliBuilder {
            a: <- numOption { short: "a" },
            b: <- numOption { short: "b" },
        }
        |> finishSubcommand { name: "ss1", description: "", mapper: SS1 }

    subSubcommandParser2 =
        cliBuilder {
            a: <- numOption { short: "a" },
            c: <- numOption { short: "c" },
        }
        |> finishSubcommand { name: "ss2", description: "", mapper: SS2 }

    subcommandParser1 =
        cliBuilderWithSubcommands {
            sc: <- subcommandField [subSubcommandParser1, subSubcommandParser2],
            d: <- numOption { short: "d" },
            e: <- numOption { short: "e" },
        }
        |> finishSubcommand { name: "s1", description: "", mapper: S1 }

    subcommandParser2 =
        cliBuilder {
            d: <- numOption { short: "d" },
            f: <- numOption { short: "f" },
        }
        |> finishSubcommand { name: "s2", description: "", mapper: S2 }

    result =
        cliBuilderWithSubcommands {
            sc: <- subcommandField [subcommandParser1, subcommandParser2],
            x: <- numOption { short: "x" },
            y: <- strParam { name: "y" },
        }
        |> finishCli { name: "app" }

    when result is
        Ok { parser, config } -> { parser, config }
        Err err -> crash "Err: $(Inspect.toStr err)"
