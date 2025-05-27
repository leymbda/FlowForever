namespace FlowForever.Domain

[<RequireQualifiedAccess>]
type Cell =
    | Empty
    | Start of lineIndex: int
    | End of lineIndex: int
    | Path of lineIndex: int

// TODO: Determine clean approach to storing directionality of cell (enum of all possible shapes?)
