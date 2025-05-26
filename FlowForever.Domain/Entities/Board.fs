namespace FlowForever.Domain

type Board = {
    Dimensions: int * int
    Lines: Line list
}

module Board =
    /// Get the percentage of the board that is filled by completed lines. Does not check for cell conflicts.
    let getFilledAmount board =
        board.Lines
        |> List.collect (Line.tryGetCompletePath >> function | Some p -> p | None -> [])
        |> List.distinct
        |> List.length
        |> decimal
        |> (/) (fst board.Dimensions * snd board.Dimensions |> decimal)

    /// Determines if the board is successfully completed. It is considered completed if all paths are valid, contain
    /// no overlaps, and the entire board is filled.
    let isCompleted board =
        let isFilled =
            getFilledAmount board = 1m

        let paths =
            board.Lines
            |> List.collect (Line.tryGetCompletePath >> function | Some p -> p | None -> [])

        let hasConflicts =
            List.length paths <> List.length (List.distinct paths)

        isFilled && not hasConflicts

    /// Attempts to add the given coordinate to the given line. If it intersects a different line, that line is sliced
    /// to end before that coordinate. If it intersects itself, the path is sliced to before the coordinate, then the
    /// coordinate is appended (resulting in the excess path being removed)..
    let trySetCell coordinate lineIndex board =
        let conflictingIndex =
            board.Lines
            |> List.map Line.getCurrentPath
            |> List.tryFindIndex (fun l -> l |> List.contains coordinate)

        let apply index f lines =
            match index with
            | None -> lines
            | Some index -> lines |> List.mapi (fun i v -> if i = index then f v else v)

        let updated =
            board.Lines
            |> apply conflictingIndex (Line.trySlice coordinate)
            |> apply (Some lineIndex) (Line.tryAddCoordinate coordinate)

        { board with Lines = updated }
