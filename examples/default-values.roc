app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    weaver: "../package/main.roc",
}

import pf.Arg
import pf.Stdout
import weaver.Opt
import weaver.Cli
import weaver.Param

main! = \args ->
    data =
        Cli.parse_or_display_message(cli_parser, args, Arg.to_os_raw)
        |> try(Result.on_err!, \message ->
            try(Stdout.line!, message)
            Err(Exit(1, "")))

    try(Stdout.line!, "Successfully parsed! Here's what I got:")
    try(Stdout.line!, "")
    try(Stdout.line!, Inspect.to_str(data))

    Ok({})

cli_parser =
    { Cli.weave <-
        alpha: Opt.u64({
            short: "a",
            long: "alpha",
            help: "Set the alpha level. [default: 123]",
            default: Value(123),
        }),
        beta: Opt.dec({
            short: "b",
            long: "beta",
            help: "Set the beta level. [default: PI]",
            default: Generate(\{} -> Num.pi),
        }),
        file: Param.maybe_str({
            name: "file",
            help: "The file to process. [default: NONE]",
        })
        |> Cli.map(\f -> Result.with_default(f, "NONE")),
    }
    |> Cli.finish({
        name: "default-values",
        version: "v0.0.1",
    })
    |> Cli.assert_valid
