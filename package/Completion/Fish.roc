module [generateCompletion]

# import Base exposing [CliConfig]
# https://fishshell.com/docs/current/completions.html
# https://medium.com/@fabioantunes/a-guide-for-fish-shell-completions-485ac04ac63c

numTypeName = "num"

firstLine = \s ->
    s
    |> Str.split "\n"
    |> List.first
    |> Result.withDefault ""

# generateCompletion : CliConfig -> Str
generateCompletion = \config ->
    options =
        config.options
        |> List.map optionCompletion
        |> Str.joinWith "\n"

    """
    complete
        -c $(config.name)
    $(options)
    """

optionCompletion = \option ->
    shortText =
        if Str.isEmpty option.short then
            "-s $(option.short)"
        else
            ""

    longText =
        if Str.isEmpty option.long then
            "-l $(option.long)"
        else
            ""

    valueTypeText =
        when option.expectedValue is
            ExpectsValue type if type == numTypeName ->
                "--no-files"

            _other -> ""

    [shortText, longText, valueTypeText]
    |> List.dropIf Str.isEmpty
    |> Str.joinWith " "
    |> Str.withPrefix "    "
