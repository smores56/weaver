interface Utils
    exposes [strLen, isKebabCase]
    imports []

strLen : Str -> U64
strLen = \s -> List.len (Str.toUtf8 s)

isDigit : U8 -> Bool
isDigit = \char ->
    zeroAsciiCode = 48
    nineAsciiCode = 57

    char >= zeroAsciiCode && char <= nineAsciiCode

isLowerCase : U8 -> Bool
isLowerCase = \char ->
    lowerAAsciiCode = 97
    lowerZAsciiCode = 122

    char >= lowerAAsciiCode && char <= lowerZAsciiCode

isKebabCase : Str -> Bool
isKebabCase = \s ->
    dashAsciiCode : U8
    dashAsciiCode = 45

    when Str.toUtf8 s is
        [] -> Bool.false
        [single] -> isLowerCase single || isDigit single
        [first, .. as middle, last] ->
            firstIsKebab = isLowerCase first
            lastIsKebab = isLowerCase last || isDigit last
            middleIsKebab =
                middle
                |> List.all \char ->
                    isLowerCase char || isDigit char || char == dashAsciiCode
            noDoubleDashes =
                middle
                |> List.map2 (List.dropFirst middle 1) Pair
                |> List.all \Pair left right ->
                    !(left == dashAsciiCode && right == dashAsciiCode)

            firstIsKebab && lastIsKebab && middleIsKebab && noDoubleDashes
