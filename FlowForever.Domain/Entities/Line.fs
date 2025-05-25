namespace FlowForever.Domain

type Line = {
    Start: Coordinate
    End: Coordinate
    Path: Coordinate list
}

module Line =
    /// Create a new line. Returns None if the start and end coordinates are touching.
    let tryCreate start end' =
        match Coordinate.isTouching start end' with
        | false -> Some { Start = start; End = end'; Path = [] }
        | true -> None

    /// Attempt to add a coordinate to the line's path. If the coordinate is not a valid continuation of the path, it
    /// will not be added. If the coordinate is already in the path, the path will be sliced to that point.
    let tryAddCoordinate coordinate line =
        let current =
            line.Path
            |> List.rev
            |> List.tryHead
            |> Option.defaultValue line.Start

        let path =
            match Coordinate.isTouching current coordinate, List.tryFindIndex ((=) coordinate) line.Path with
            | true, Some i -> line.Path |> List.take (i + 1)
            | true, None -> coordinate :: line.Path
            | false, _ -> line.Path

        { line with Path = path }

    /// Attempt to slice the path at the given coordinate, removing it as well. If the coordinate is not found in the
    /// path, the line remains unchanged.
    let trySlice coordinate line =
        match line.Path |> List.tryFindIndex ((=) coordinate) with
        | Some i -> { line with Path = line.Path |> List.take i }
        | _ -> line

    /// Attempt to slice the path at the given coordinate, keeping it in the path. If the coordinate is not found in
    /// the path, the line remains unchanged.
    let trySliceTo coordinate line =
        match line.Path |> List.tryFindIndex ((=) coordinate) with
        | Some i -> { line with Path = line.Path |> List.take (i + 1) }
        | _ -> line

    /// Check if the line is completed by connecting the start and end point through the path. A line is not valid if
    /// any point in its path touches more than two points, as this means it is passing by itself, or if the path is
    /// disjointed, which should not occur if the record is only modified through provided functions.
    let isConnected line =
        let complete = line.Start :: (line.Path @ [line.End])

        let isValidStart =
            line.Path
            |> List.tryHead
            |> Option.map (Coordinate.isTouching line.Start)
            |> Option.defaultValue false

        let isValidPath =
            complete
            |> List.mapi (fun i c ->
                match i = 0 || (i = List.length complete - 1) with
                | true -> []
                | false -> [complete[i - 1], c, complete[i + 1]]
            )
            |> List.collect id
            |> List.forall (fun (pre, cur, nex) -> Coordinate.isTouching pre cur && Coordinate.isTouching cur nex)
            
        let isValidEnd =
            line.Path
            |> List.tryLast
            |> Option.map (Coordinate.isTouching line.End)
            |> Option.defaultValue false
            
        let isPathOverlapping =
            line.Path
            |> List.exists (fun c ->
                complete
                |> List.filter (Coordinate.isTouching c)
                |> List.length
                |> (<>) 2
            )

        isValidStart && isValidPath && isValidEnd && not isPathOverlapping
