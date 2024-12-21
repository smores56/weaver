module [str_len, is_kebab_case, to_upper_case]

# TODO: this is a terrible way to check string length!
str_len : Str -> U64
str_len = \s -> List.len (Str.toUtf8 s)

is_digit : U8 -> Bool
is_digit = \char ->
    zero_ascii_code = 48
    nine_ascii_code = 57

    char >= zero_ascii_code && char <= nine_ascii_code

is_lower_case : U8 -> Bool
is_lower_case = \char ->
    char >= 'a' && char <= 'z'

is_kebab_case : Str -> Bool
is_kebab_case = \s ->
    dash_ascii_code : U8
    dash_ascii_code = 45

    when Str.toUtf8 s is
        [] -> Bool.false
        [single] -> is_lower_case single || is_digit single
        [first, .. as middle, last] ->
            first_is_kebab = is_lower_case first
            last_is_kebab = is_lower_case last || is_digit last
            middle_is_kebab =
                middle
                |> List.all \char ->
                    is_lower_case char || is_digit char || char == dash_ascii_code
            no_double_dashes =
                middle
                |> List.map2 (List.dropFirst middle 1) Pair
                |> List.all \Pair left right ->
                    !(left == dash_ascii_code && right == dash_ascii_code)

            first_is_kebab && last_is_kebab && middle_is_kebab && no_double_dashes

to_upper_case : Str -> Str
to_upper_case = \str ->
    str
    |> Str.toUtf8
    |> List.map \c ->
        if is_lower_case c then
            c - ('a' - 'A')
        else
            c
    |> Str.fromUtf8
    |> Result.withDefault ""

expect str_len "123" == 3

expect
    sample = "19aB "

    sample
    |> Str.toUtf8
    |> List.map is_digit
    == [Bool.true, Bool.true, Bool.false, Bool.false, Bool.false]

expect
    sample = "aAzZ-"

    sample
    |> Str.toUtf8
    |> List.map is_lower_case
    == [Bool.true, Bool.false, Bool.true, Bool.false, Bool.false]

expect is_kebab_case "abc-def"
expect is_kebab_case "-abc-def" |> Bool.not
expect is_kebab_case "abc-def-" |> Bool.not
expect is_kebab_case "-" |> Bool.not
expect is_kebab_case "" |> Bool.not

expect to_upper_case "abc" == "ABC"
expect to_upper_case "ABC" == "ABC"
expect to_upper_case "aBc00-" == "ABC00-"
