module [strLen, isKebabCase, toUpperCase]

lower_a_ascii_code = 97
lower_z_ascii_code = 122
lower_to_upper_case_ascii_delta = 32

# TODO: this is a terrible way to check string length!
str_len : Str -> U64
str_len = \s -> List.len (Str.toUtf8 s)

is_digit : U8 -> Bool
is_digit = \char ->
    zero_ascii_code = 48
    nine_ascii_code = 57

    char >= zeroAsciiCode && char <= nineAsciiCode

is_lower_case : U8 -> Bool
is_lower_case = \char ->
    char >= lowerAAsciiCode && char <= lowerZAsciiCode

is_kebab_case : Str -> Bool
is_kebab_case = \s ->
    dash_ascii_code : U8
    dash_ascii_code = 45

    when Str.toUtf8 s is
        [] -> Bool.false
        [single] -> isLowerCase single || isDigit single
        [first, .. as middle, last] ->
            first_is_kebab = isLowerCase first
            last_is_kebab = isLowerCase last || isDigit last
            middle_is_kebab =
                middle
                |> List.all \char ->
                    isLowerCase char || isDigit char || char == dashAsciiCode
            no_double_dashes =
                middle
                |> List.map2 (List.dropFirst middle 1) Pair
                |> List.all \Pair left right ->
                    !(left == dashAsciiCode && right == dashAsciiCode)

            firstIsKebab && lastIsKebab && middleIsKebab && noDoubleDashes

to_upper_case : Str -> Str
to_upper_case = \str ->
    str
    |> Str.toUtf8
    |> List.map \c ->
        if isLowerCase c then
            c - lowerToUpperCaseAsciiDelta
        else
            c
    |> Str.fromUtf8
    |> Result.withDefault ""

expect strLen "123" == 3

expect
    sample = "19aB "

    sample
    |> Str.toUtf8
    |> List.map isDigit
    == [Bool.true, Bool.true, Bool.false, Bool.false, Bool.false]

expect
    sample = "aAzZ-"

    sample
    |> Str.toUtf8
    |> List.map isLowerCase
    == [Bool.true, Bool.false, Bool.true, Bool.false, Bool.false]

expect isKebabCase "abc-def"
expect isKebabCase "-abc-def" |> Bool.not
expect isKebabCase "abc-def-" |> Bool.not
expect isKebabCase "-" |> Bool.not
expect isKebabCase "" |> Bool.not

expect toUpperCase "abc" == "ABC"
expect toUpperCase "ABC" == "ABC"
expect toUpperCase "aBc00-" == "ABC00-"
