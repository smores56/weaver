Weaver
======

An ergonomic command-line argument parser for the Roc language.

This library aims to provide a convenient interface for parsing CLI arguments
into structured data, in the style of Rust's [clap](https://github.com/clap-rs/clap).
Without code generation at compile time, the closest we can get in Roc is the use of the
[record builder syntax](https://www.roc-lang.org/examples/RecordBuilder/README.html).
This allows us to build our config and parser at the same time, in a type-safe way!

Read the documentation at <https://smores56.github.io/weaver/Cli/>.

## Status

An initial release has been made, so you can use Weaver right now! Just grab the download
URL from the [latest GitHub release](https://github.com/smores56/weaver/releases/tag/0.1.0)
and import it into your app.

More niceties will be added in the next few weeks, but the general structure of the library
is unlikely to change much unless I find a big improvement from a different parsing strategy.

## Example

```roc
app "basic"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        weaver: "https://github.com/smores56/weaver/releases/download/0.1.0/MnJi0GTNzOI77qDnH99iuBNsM5ZKnc-gZTLFj7sIdqo.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Arg,
        pf.Task.{ Task },
        weaver.Opt,
        weaver.Cli,
        weaver.Param,
        weaver.Subcommand,
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await

    textToDisplay =
        when Cli.parseOrDisplayMessage cliParser args is
            Ok data -> "Successfully parsed! Here's what I got:\n\n$(Inspect.toStr data)"
            Err message -> message

    Stdout.line textToDisplay

cliParser =
    subSubcommandParser1 =
        Cli.weave {
            a: <- Opt.num { short: "a", help: "An example short flag for a sub-subcommand." },
            b: <- Opt.num { short: "b", help: "Another example short flag for a sub-subcommand." },
        }
        |> Subcommand.finish { name: "ss1", description: "A sub-subcommand.", mapper: SS1 }

    subSubcommandParser2 =
        Cli.weave {
            a: <- Opt.num { short: "a", help: "Set the alpha level." },
            c: <- Opt.num { short: "c", long: "create", help: "Create a doohickey." },
            data: <- Param.str { name: "data", help: "Data to manipulate." },
        }
        |> Subcommand.finish { name: "ss2", description: "Another sub-subcommand.", mapper: SS2 }

    subcommandParser1 =
        Cli.weave {
            d: <- Opt.maybeNum { short: "d", help: "A non-overlapping subcommand flag with s2." },
            volume: <- Opt.maybeNum { short: "v", long: "volume", help: "How loud to grind the gears." },
            sc: <- Subcommand.field [subSubcommandParser1, subSubcommandParser2],
        }
        |> Subcommand.finish { name: "s1", description: "A first subcommand.", mapper: S1 }

    subcommandParser2 =
        Cli.weave {
            d: <- Opt.maybeNum { short: "d", help: "This doesn't overlap with s1's -d flag." },
        }
        |> Subcommand.finish {
            name: "s2",
            description: "Another subcommand.",
            mapper: S2,
        }

    Cli.weave {
        force: <- Opt.flag { short: "f", help: "Force the task to complete." },
        sc: <- Subcommand.field [subcommandParser1, subcommandParser2],
        file: <- Param.maybeStr { name: "file", help: "The file to process." },
        files: <- Param.strList { name: "files", help: "The rest of the files." },
    }
    |> Cli.finish {
        name: "basic",
        version: "v0.0.1",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    }
    |> Cli.assertValid
```

And here's us calling the above example from the command line:

```console
$ roc readme.roc -- file1.txt file2.txt -f
Successfully parsed! Here's what I got:

{file: (Ok "file1.txt"), files: ["file2.txt"], force: Bool.true, sc: (Err NoSubcommand)}

$ roc readme.roc -- --help
basic v0.0.1
Some One <some.one@mail.com>

This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!

Usage:
  basic [OPTIONS] [file] [files]...
  basic <COMMAND>

Commands:
  s1  A first subcommand.
  s2  Another subcommand.

Arguments:
  [file]      The file to process.
  [files]...  The rest of the files.

Options:
  -f             Force the task to complete.
  -h, --help     Show this help page.
  -V, --version  Show the version.
```

There are also some examples in the [examples](./examples) directory that are more feature-complete,
with more to come as this library matures.

## Roadmap

Now that an initial release has happened, these are some ideas I have for future development:

- [ ] Set default values in the arguments themselves
- [ ] Nested option records that all parse under a single command for better modularity
- [ ] Optionally set `{ group : Str }` per option so they are visually grouped in the help page
- [ ] Completion generation for popular shells (e.g. Bash, Zsh, Fish, etc.)
- [ ] Add terminal escape sequences to generated messages for prettier help/usage text formatting
- [ ] add convenient `Task` helpers (e.g. parse or print help and exit) once [module params](https://docs.google.com/document/u/0/d/110MwQi7Dpo1Y69ECFXyyvDWzF4OYv1BLojIm08qDTvg) land
- [ ] Clean up default parameter code if we can elide different fields on the same record type in different places (not currently allowed)
- [ ] Add more testing (always)
