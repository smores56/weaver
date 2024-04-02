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
    args = ["app", "-a", "123", "-b", "--xyz", "some_text"]
    out =
        cliBuilder args { name: "app" } {
            a: <- numOption { short: "a" },
            b: <- flagOption { short: "b" },
            xyz: <- strOption { long: "some_text" },
        }
        |> finishOrErr

    out == Ok { a: 123, b: Bool.true, xyz: "some_text" }
```

## Roadmap

This library is barely even work-in-progress, but once I can get an example running
with the [basic-cli](https://github.com/roc-lang/basic-cli) platform, these are the
main features I want to add support for:

- [ ] simply-implemented support for optional args/lists of args
- [ ] automatic help text generation
- [ ] subcommands, as simple as possible
- [ ] choice args that select an option from a custom enum
- [ ] maybe completion generation at some point?
