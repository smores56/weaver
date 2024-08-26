app [main] {
    pf: platform "../../basic-cli/platform/main.roc",
    weaver: "../package/main.roc",
}

import pf.Stdout
import pf.Arg
import weaver.Opt
import weaver.Cli
import weaver.Param

main =
    args = Arg.list! {}

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
        alpha: Opt.u64 { short: "a", help: "Set the alpha level." },
        force: Opt.flag { short: "f", help: "Force the task to complete." },
        file: Param.maybeStr { name: "file", help: "The file to process." },
        files: Param.strList { name: "files", help: "The rest of the files." },
    }
    |> Cli.finish {
        name: "basic",
        version: "v0.0.1",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    }
    |> Cli.assertValid
