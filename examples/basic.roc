app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    weaver: "../package/main.roc",
}

import pf.Arg
import pf.Stdout
import weaver.Opt
import weaver.Cli
import weaver.Param

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
        alpha: Opt.u64 { short: "a", help: "Set the alpha level." },
        force: Opt.flag { short: "f", help: "Force the task to complete." },
        file: Param.maybe_str { name: "file", help: "The file to process." },
        files: Param.str_list { name: "files", help: "The rest of the files." },
    }
    |> Cli.finish {
        name: "basic",
        version: "v0.0.1",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    }
    |> Cli.assert_valid
