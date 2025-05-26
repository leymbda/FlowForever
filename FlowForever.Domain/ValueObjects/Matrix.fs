namespace FlowForever.Domain

type Matrix<'T> = List<List<'T>>

module Matrix =
    let init w h f: Matrix<'T> =
        List.init h (fun y -> List.init w (fun x -> f x y))

    let map f (matrix: Matrix<'T1>): Matrix<'T2> =
        matrix |> List.mapi (fun y row ->
            row |> List.mapi (fun x v ->
                f x y v
            )
        )
