Weaver
======

An ergonomic command-line argument parser for the Roc language.

This library aims to provide a convenient interface for parsing CLI arguments
into structured data, in the style of Rust's [clap](https://github.com/clap-rs/clap).
Without code generation at compile time, the closest we can get in Roc is the use of the
[record builder syntax](https://www.roc-lang.org/examples/RecordBuilder/README.html).
This allows us to build our config and parser at the same time, in a type-safe way!

## Status

This library is mostly built out for the basic features, but there are still some logic bugs that I need
to iron out (ignoring compiler issues that I'm trying not to get blocked on). There will still be some
breaking changes in the next few weeks, but it's ready for alpha testing (more or less) if you're feeling
brave! Once I think the API is where I want it, I'll start making GitHub releases.

Documentation is the next thing on my ticket, so look out for that in the next few days!

## Example

```roc
expect
    subcommandParser =
        cliBuilder {
            d: <- numOption { name: Short "d", help: "A required number." },
            f: <- maybeNumOption { name: Short "f", help: "An optional number." },
        }
        |> finishSubcommand { name: "sub", description: "A specific action to take.", mapper: Sub }

    { parser, config: _ } =
        cliBuilder {
            alpha: <- numOption { name: Short "a", help: "Set the alpha level." },
            beta: <- flagOption { name: Both "b" "beta" },
            xyz: <- strOption { name: Long "xyz" },
            verbosity: <- occurrenceOption { name: Both "v" "verbose" },
            sc: <- subcommandField [subcommandParser],
        }
        |> finishCli { name: "app", version: "v0.0.1", authors: ["Some One <some.one@mail.com>"] }
        |> assertCliIsValid # crash immediately on unrecoverable issues, e.g. empty flag names

    out = parser ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv", "sub", "-d", "456"]

    out
    == SuccessfullyParsed {
        alpha: 123,
        beta: Bool.true,
        xyz: "some_text",
        verbosity: 4,
        sc: Ok (Sub { d: 456, f: Err NoValue }),
    }
```

There are also some examples in the [examples](./examples) directory that are more feature-complete,
with more to come as this library matures.

## Roadmap

Beyond finishing up the basics, these are the main things I want to work on:

- [X] simply-implemented support for optional args/lists of args
- [ ] full documentation of the library's features
- [ ] multiple documented and tested examples
- [X] automatic help text generation
- [X] subcommands, as simple as possible
- [X] choice args that select an option from a custom enum
- [ ] add more testing
- [ ] maybe add option groups (optionally set { group : Str } per option)
- [ ] CI/CD testing and deployment, respectively

### Long-Term Goals

- [ ] add convenient `Task` helpers (e.g. parse or print help and exit) once [module params](https://docs.google.com/document/u/0/d/110MwQi7Dpo1Y69ECFXyyvDWzF4OYv1BLojIm08qDTvg) land
- [ ] Completion generation for popular shells
- [ ] Clean up default parameter code if we can elide different fields on the same record type in different places (not currently allowed)
