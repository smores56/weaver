module []

import Opt
import Cli

basicCli =
    Opt.u64 { short: "a", help: "Set the alpha level." }
    |> Cli.map Alpha
    |> Cli.finish { name: "basic-cli", version: "v1.0.0", textStyle: Plain }
    |> Cli.assertValid

expect
    basicCli
    |> Cli.parseOrDisplayMessage ["basic-cli", "-a", "123"]
    == Ok (Alpha 123)

expect
    helpMessage =
        """
        basic-cli v1.0.0

        Usage:
          basic-cli -a NUM [options]

        Options:
          -a NUM         Set the alpha level.
          -h, --help     Show this help page.
          -V, --version  Show the version.
        """

    basicCli
    |> Cli.parseOrDisplayMessage ["basic-cli", "-h"]
    == Err helpMessage
