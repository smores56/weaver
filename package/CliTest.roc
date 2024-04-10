interface CliTest
    exposes []
    imports [Opt, Cli]

basicCli =
    Cli.weave {
        a: <- Opt.num { short: "a", help: "Set the alpha level." },
    }
    |> Cli.finish { name: "basic-cli", version: "v1.0.0" }
    |> Cli.assertValid

expect
    basicCli
    |> Cli.parseOrDisplayMessage ["basic-cli", "-a", "123"]
    == Ok { a: 123 }

expect
    helpMessage =
        """
        basic-cli v1.0.0

        No description.

        Usage:
          basic-cli [OPTIONS]

        Options:
          -a NUM         Set the alpha level.
          -h, --help     Show this help page.
          -V, --version  Show the version.
        """

    basicCli
    |> Cli.parseOrDisplayMessage ["basic-cli", "-h"]
    == Err helpMessage
