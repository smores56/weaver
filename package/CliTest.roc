interface CliTest
    exposes []
    imports [Opt, Cli]

basicCli =
    Cli.weave {
        a: <- Opt.num { short: "a" },
    }
    |> Cli.finish { name: "basic-cli" }
    |> Cli.assertValid

expect
    basicCli
    |> Cli.parseOrDisplayMessage ["basic-cli", "-a", "123"]
    == Ok { a: 123 }
