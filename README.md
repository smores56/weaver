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

This library is ready to parse your args today, but I'm always looking for more testing
from the community! Feel free to open a GitHub issue if there's a feature you're missing
from another CLI parsing library that you think would fit well in Weaver's nest.

## Example

```roc
app [main!] {
    pf: platform "<latest from https://github.com/roc-lang/basic-cli/releases>",
    weaver: "<latest from https://github.com/smores56/weaver/releases>",
}

import pf.Arg
import pf.Stdout
import weaver.Opt
import weaver.Cli
import weaver.Param

main! = \args ->
    data =
        Cli.parse_or_display_message cli_parser args Arg.to_os_raw
        |> try Result.onErr! \message ->
            try Stdout.line! message
            Err (Exit 1 "")

    try Stdout.line! "Successfully parsed! Here's what I got:"
    try Stdout.line! ""
    try Stdout.line! (Inspect.toStr data)

    Ok {}

cli_parser =
    { Cli.weave <-
        alpha: Opt.u64 { short: "a", help: "Set the alpha level." },
        force: Opt.flag { short: "f", help: "Force the task to complete." },
        file: Param.maybe_str { name: "file", help: "The file to process." },
        files: Param.str_list { name: "files", help: "The rest of the files." },
    }
    |> Cli.finish {
        name: "basic",
        version: "v0.1.0",
        authors: ["Some One <some.one@mail.com>"],
        description: "This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!",
    }
    |> Cli.assert_valid
```

And here's us calling the above example from the command line:

```console
$ roc examples/basic.roc -- file1.txt file2.txt -f -a 123
Successfully parsed! Here's what I got:

{alpha: 123, file: (Ok "file1.txt"), files: ["file2.txt"], force: Bool.true}

$ roc readme.roc -- --help
basic v0.0.1
Some One <some.one@mail.com>

This is a basic example of what you can build with Weaver. You get safe parsing, useful error messages, and help pages all for free!

Usage:
  basic -a NUM [options] <file> <files...>

Arguments:
  <file>      The file to process.
  <files...>  The rest of the files.

Options:
  -a NUM         Set the alpha level.
  -f             Force the task to complete.
  -h, --help     Show this help page.
  -V, --version  Show the version.
```

There are also some examples in the [examples](./examples) directory that are more
feature-complete, with more to come as this library matures.

## Roadmap

Now that an initial release has happened, these are some ideas I have for future development:

- [ ] Optionally set `{ group ? Str }` per option so they are visually grouped in the help page
- [ ] Completion generation for popular shells (e.g. Bash, Zsh, Fish, etc.)
- [X] Add terminal escape sequences to generated messages for prettier help/usage text formatting (currently working, but could be nicer/more configurable)
- [ ] add convenient CLI platform wrappers (e.g. parse, or print help and exit) for use with module params
- [X] Clean up default parameter code if we can elide different fields on the same record type in different places (not currently allowed)
- [ ] Add more testing (always)
