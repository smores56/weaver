module [
    Arg,
    from_raw_arg,
    to_raw_arg,
    from_str,
    to_str,
    to_bytes,
    display,
]

## An OS-aware representation of a command-line argument.
##
## Though we tend to think of args as Unicode strings, most operating systems
## represent command-line arguments as lists of bytes that aren't necessarily
## UTF-8 encoded. Windows doesn't even use bytes, but U16s.
##
## Most of the time, you will pass these to packages and they will handle the
## encoding for you, but for quick-and-dirty code you can use [display] to
## convert these to [Str] in a lossy way.
Arg := [Unix (List U8), Windows (List U16)] implements [Eq, Inspect { to_inspector: arg_inspector }]

arg_inspector : Arg -> Inspector f where f implements InspectFormatter
arg_inspector = \arg -> Inspect.str(display(arg))

## Wrap a raw, OS-aware numeric list into an [Arg].
from_raw_arg : [Unix (List U8), Windows (List U16)] -> Arg
from_raw_arg = \raw_arg -> @Arg(raw_arg)

## Unwrap an [Arg] into a raw, OS-aware numeric list.
##
## This is a good way to pass [Arg]s to Roc packages.
to_raw_arg : Arg -> [Unix (List U8), Windows (List U16)]
to_raw_arg = \@Arg(raw_arg) -> raw_arg

## Encode a UTF-8 [Str] to a Unix-flavored [Arg].
from_str : Str -> Arg
from_str = \str ->
    @Arg(Unix(Str.to_utf8(str)))

## Attempt to decode an [Arg] to a UTF-8 [Str].
to_str : Arg -> Result Str [InvalidUtf8]
to_str = \@Arg(arg) ->
    # TODO: update when Unicode -> Str conversion is ready:
    # https://github.com/roc-lang/roc/issues/7390
    when arg is
        Unix(unix) ->
            Str.from_utf8(unix)
            |> Result.map_err(\_err -> InvalidUtf8)

        Windows(_windows) -> Err(InvalidUtf8)

## Convert an [Arg] to a list of bytes.
to_bytes : Arg -> List U8
to_bytes = \@Arg(arg) ->
    when arg is
        Unix(unix) -> unix
        Windows(windows) ->
            # avoid intermediate list resizing allocations by
            # appending to a list instead of using `List.join_map`
            helper = \codepoints, bytes ->
                when codepoints is
                    [] -> bytes
                    [codepoint, .. as rest] ->
                        lower = codepoint |> Num.to_u8
                        upper =
                            codepoint
                            |> Num.shift_right_by(8)
                            |> Num.to_u8

                        updated_bytes =
                            bytes
                            |> List.append(upper)
                            |> List.append(lower)

                        helper(rest, updated_bytes)

            bytes_out = List.with_capacity((2 * List.len(windows)))

            helper(windows, bytes_out)

## Convert an Arg to a `Str` for display purposes.
##
## NB: This currently only supports valid UTF-8 Unix strings. Once Roc adds
## support for lossy conversion of Unicode to Str, this will replace invalid
## codepoints with the Unicode replacement character "\uFFFD".
display : Arg -> Str
display = \@Arg(arg) ->
    # TODO: update when Unicode -> Str conversion is ready:
    # https://github.com/roc-lang/roc/issues/7390
    when arg is
        Unix(unix) ->
            when Str.from_utf8(unix) is
                Ok(str) -> str
                Err(err) -> crash("Invalid UTF-8 string: $(Inspect.to_str(err))")

        Windows(_windows) ->
            crash("Windows args cannot currently be displayed")

