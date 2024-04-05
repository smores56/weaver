interface ParamsBuilder
    exposes [
        ParamsBuilder,
        paramsBuilder,
        strParam,
    ]
    imports [Config.{ Arg, ArgExtractErr, ParameterConfig }]

ParamsBuilder state := {
    parser : List Arg -> Result (state, List Arg) ArgExtractErr,
    params : List ParameterConfig,
}

paramsBuilder : base -> ParamsBuilder base
paramsBuilder = \base ->
    @ParamsBuilder {
        params: [],
        parser: \args -> Ok (base, args),
    }

getParamsFromArgs : List Arg, ParameterConfig -> Result (List Str, List Arg) ArgExtractErr
getParamsFromArgs = \args, param ->
    startingState = { action: Nothing, values: [], remainingArgs: [] }
    stateAfter = List.walkTry args startingState \state, arg ->
        when state.action is
            StopParsing ->
                Ok { state & remainingArgs: state.remainingArgs |> List.append arg }

            Nothing ->
                when arg is
                    Short s ->
                        

                    Long l ->
                        123

                    Param p ->
                        if p == "--" then
                            { state & action: StopParsing, remainingArgs: state.remainingArgs |> List.append arg }
                        else
                            { state & values: state.values |> List.append p }

    when stateAfter is
        Ok { action, values, remainingArgs } -> Ok (values, remainingArgs)
        Err err -> Err err

strParam : { name : Str, help ? Str } -> (ParamsBuilder (Str -> state) -> ParamsBuilder state)
strParam = \{ name, help ? "" } ->
    param = { name, help, type: Str, plurality: One }

    \@ParamsBuilder builder ->
        newParser = \args ->
            { data, subcommand, values, remainingArgs } <- builder.parser param args
                |> Result.try

            when values is
                [] -> Err (MissingParam param)
                [single] -> Ok ({ data: data single, subcommand }, remainingArgs)
                [..] -> Err (TooManyParamsProvided param)

        @ParamsBuilder {
            params: builder.params |> List.append param,
            parser: newParser,
        }
