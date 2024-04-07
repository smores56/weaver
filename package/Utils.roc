interface Utils
    exposes [strLen]
    imports []

strLen : Str -> U64
strLen = \s -> List.len (Str.toUtf8 s)
