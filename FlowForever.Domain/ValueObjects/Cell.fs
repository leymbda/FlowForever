namespace FlowForever.Domain

[<RequireQualifiedAccess>]
type Cell =
    | Empty
    | Start of lineIndex: int
    | End of lineIndex: int
    | Path of lineIndex: int
