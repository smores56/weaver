app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    weaver: "../package/main.roc",
}

import pf.Arg
import pf.Stdout
import weaver.Opt
import weaver.Cli
import weaver.Param

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
    { Cli.weave <-
        alpha: Opt.u64({ short: "a", help: "Set the alpha level." }),
        force: Opt.flag({ short: "f", help: "Force the task to complete." }),
        file: Param.maybe_str({ name: "file", help: "The file to process." }),
        files: Param.str_list({ name: "files", help: "The rest of the files." }),
    }
    |> Cli.finish({
        name: "basic",
        version: "v0.0.1",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    })
    |> Cli.assert_valid
