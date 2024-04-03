interface Test
    exposes []
    imports []

TestBuilder base out := {
    baseBuilder : base,
    options : List TestOption,
    ourBuilder : base, List Str -> Result (out, List Str) TestErr,
}

TestErr : [NoMoreArgs TestOption, InvalidNumArg]
TestOption : [Str, Num]

start : base -> TestBuilder base base
start = \base ->
    @TestBuilder {
        baseBuilder: base,
        options: [],
        ourBuilder: \s, args -> Ok (s, args),
    }

nextStr : TestBuilder base (Str -> state) -> TestBuilder base state
nextStr = \@TestBuilder builder ->
    @TestBuilder {
        baseBuilder: builder.baseBuilder,
        options: builder.options |> List.append Str,
        ourBuilder: \state, args ->
            (out, argsLeft) <- builder.ourBuilder state args
                |> Result.try
            first <- List.first argsLeft
                |> Result.mapErr \_ -> NoMoreArgs Str
                |> Result.try
            remaining = List.dropFirst argsLeft 1

            Ok (out first, remaining),
    }

nextNum : TestBuilder base (U64 -> state) -> TestBuilder base state
nextNum = \@TestBuilder builder ->
    @TestBuilder {
        baseBuilder: builder.baseBuilder,
        options: builder.options |> List.append Num,
        ourBuilder: \state, args ->
            (out, argsLeft) <- builder.ourBuilder state args
                |> Result.try
            first <- List.first argsLeft
                |> Result.mapErr \_ -> NoMoreArgs Num
                |> Result.try
            firstNum <- first
                |> Str.toU64
                |> Result.mapErr \_ -> InvalidNumArg
                |> Result.try
            remaining = List.dropFirst argsLeft 1

            Ok (out firstNum, remaining),
    }

finish : TestBuilder base {}out -> (List Str -> Result {}out TestErr)
finish = \@TestBuilder builder ->
    \args ->
        builder.baseBuilder
        |> builder.ourBuilder args
        |> Result.map .0

extractOptions : TestBuilder base {}out -> List TestOption
extractOptions = \@TestBuilder builder ->
    builder.options

parserBase =
    start {
        a: <- nextStr,
        b: <- nextNum,
    }

parser = finish parserBase
parserOptions = extractOptions parserBase

expect
    out = parser ["123", "456", "789"]

    out == Ok { a: "123", b: 456 }

expect
    out = parser ["123"]

    out == Err (NoMoreArgs Num)

expect
    out = parser ["abc", "def", "ghi"]

    out == Err InvalidNumArg

expect
    parserOptions == [Str, Num]
