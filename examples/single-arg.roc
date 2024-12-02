app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    weaver: "../package/main.roc",
}

import pf.Arg
import pf.Stdout
import weaver.Opt
import weaver.Cli

main! = \{} ->
    args = Arg.list! {}

    when Cli.parseOrDisplayMessage cliParser args is
        Ok data ->
            Stdout.line! "Successfully parsed! Here's what I got:"
            Stdout.line! ""
            Stdout.line! (Inspect.toStr data)

        Err message ->
            Stdout.line! message

            Err (Exit 1 "")

cliParser =
    Opt.u64 { short: "a", long: "alpha", help: "Set the alpha level." }
    |> Cli.map Alpha
    |> Cli.finish {
        name: "single-arg",
        version: "v0.0.1",
    }
    |> Cli.assertValid
