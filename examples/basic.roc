app "basic"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        roclap: "../package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Arg,
        pf.Task.{ Task },
        roclap.Builder.{ cliBuilder, numOption, strOption, flagOption, occurrenceOption, getParser },
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await

    parser =
        cliBuilder {
            alpha: <- numOption { short: "a" },
            beta: <- flagOption { short: "b", long: "--beta" },
            xyz: <- strOption { long: "xyz" },
            verbosity: <- occurrenceOption { short: "v", long: "--verbose" },
        }
        |> getParser

    when parser args is
        Ok data ->
            data
            |> Inspect.toStr
            |> Stdout.line

        Err err ->
            Stdout.line ("Error: $(Inspect.toStr err)")
