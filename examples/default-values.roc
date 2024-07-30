app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    weaver: "../package/main.roc",
}

import pf.Stdout
import pf.Arg
import pf.Task exposing [Task]
import weaver.Opt
import weaver.Cli
import weaver.Param

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
    { Cli.weave <-
        alpha: Opt.u64 {
            short: "a",
            long: "alpha",
            help: "Set the alpha level. [default: 123]",
            default: Value 123,
        },
        beta: Opt.dec {
            short: "b",
            long: "beta",
            help: "Set the beta level. [default: PI]",
            default: Generate (\{} -> Num.pi),
        },
        file: Param.maybeStr {
            name: "file",
            help: "The file to process. [default: NONE]",
        }
        |> Cli.map \f -> Result.withDefault f "NONE",
    }
    |> Cli.finish {
        name: "default-values",
        version: "v0.0.1",
    }
    |> Cli.assertValid
