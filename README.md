roclap
======

A roc command-line argument parser (name pending).

This library aims to provide a convenient interface for parsing CLI arguments
into structured data, in the style of Rust's [clap](https://github.com/clap-rs/clap).
Without code generation, the closest we can get in Roc is the use of the
[record builder syntax](https://www.roc-lang.org/examples/RecordBuilder/README.html).
This allows us to build our config and parse at the same time, in a type-safe way!

## Example

There is a type-checking (but currently crashing) example in the (examples)[./examples]
directory, but here's a snippet that shows the basics:

```roc
expect
    parser =
        cliBuilder {
            alpha: <- numOption { short: "a" },
            beta: <- flagOption { short: "b", long: "--beta" },
            xyz: <- strOption { long: "xyz" },
            verbosity: <- occurrenceOption { short: "v", long: "--verbose" },
        }
        |> getParser

    out = parser ["app", "-a", "123", "-b", "--xyz", "some_text", "-vvvv"]

    out == Ok { alpha: 123, beta: Bool.true, xyz: "some_text", verbosity: 4 }
```

## Roadmap

This library is barely even work-in-progress, but once I can get an example running
with the [basic-cli](https://github.com/roc-lang/basic-cli) platform, these are the
main features I want to add support for:

- [X] simply-implemented support for optional args/lists of args
- [ ] full documentation of the library's features
- [ ] multiple documented and tested examples
- [ ] automatic help text generation
- [ ] subcommands, as simple as possible
- [X] choice args that select an option from a custom enum
- [ ] maybe completion generation at some point?
