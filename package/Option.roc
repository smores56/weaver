interface Option
    exposes [
        CliOption,
        OptionName,
        OptionHelp,
        OptionBase,
        ChoiceOptionConfig,
        optionName,
    ]
    imports []

OptionName a : { short ? Str, long ? Str, name ? Str }a
OptionHelp a : { help ? Str }a
OptionBase a : OptionName OptionHelp a

CliOption : [
    Str (OptionBase {}),
    Num (OptionBase {}),
    MaybeStr (OptionBase {}),
    MaybeNum (OptionBase {}),
    StrList (OptionBase {}),
    NumList (OptionBase {}),
    Flag (OptionBase {}),
    Frequency (OptionBase {}),
    Choice ChoiceOptionConfig,
]

ChoiceOptionConfig a : OptionBase {
    variants : []a,
    onMissing ? [UseDefault []a, Fail],
    parser : Str -> Result []a [InvalidOption],
}

optionName : CliOption -> Str
optionName = \option ->
    base =
        when option is
            Str config -> config
            Num config -> config
            MaybeStr config -> config
            MaybeNum config -> config
            StrList config -> config
            NumList config -> config
            Flag config -> config
            Frequency config -> config
            Choice config -> config

    { short ? "", long ? "", name ? "" } = base

    if name != "" then
        name
    else if long != "" then
        long
    else
        short
