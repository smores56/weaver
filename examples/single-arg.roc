app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    weaver: "../package/main.roc",
}

import pf.Arg
import pf.Stdout
import weaver.Opt
import weaver.Cli

main! = |args|
    data =
        Cli.parse_or_display_message(cli_parser, args, Arg.to_os_raw)
        |> Result.on_err!(|message|
            Stdout.line!(message)?
            Err(Exit(1, ""))
        )

    Stdout.line!("Successfully parsed! Here's what I got:")?
    Stdout.line!("")?
    Stdout.line!(Inspect.to_str(data))?

    Ok({})

cli_parser =
    Opt.u64({ short: "a", long: "alpha", help: "Set the alpha level." })
    |> Cli.map(Alpha)
    |> Cli.finish({
        name: "single-arg",
        version: "v0.0.1",
    })
    |> Cli.assert_valid
