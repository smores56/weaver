module []

import Opt
import Cli

basic_cli =
    Opt.u64 { short: "a", help: "Set the alpha level." }
    |> Cli.map Alpha
    |> Cli.finish { name: "basic-cli", version: "v1.0.0", text_style: Plain }
    |> Cli.assert_valid

expect
    basic_cli
    |> Cli.parse_or_display_message ["basic-cli", "-a", "123"]
    == Ok (Alpha 123)

expect
    help_message =
        """
        basic-cli v1.0.0

        Usage:
          basic-cli -a NUM [options]

        Options:
          -a NUM         Set the alpha level.
          -h, --help     Show this help page.
          -V, --version  Show the version.
        """

    basic_cli
    |> Cli.parse_or_display_message ["basic-cli", "-h"]
    == Err help_message
