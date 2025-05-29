open FlowForever.Domain

let print board =
    Board.matrix board
    |> List.map (
        List.map (
            function
            | Cell.Empty -> " "
            | Cell.Start i
            | Cell.End i
            | Cell.Path i -> string i // TODO: This will break on numbers greater than 9
        )
        >> List.fold (+) ""
        >> printfn "%s"
    )
