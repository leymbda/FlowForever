namespace FlowForever.Domain

type Coordinate = {
    X: int
    Y: int
}

module Coordinate =
    /// Returns if the two coordinates are touching either vertically or horizontally but not both at the same time. If
    /// the coordinates are the same, they are considered not touching.
    let isTouching (c1: Coordinate) (c2: Coordinate) =
        let dx = abs (c1.X - c2.X)
        let dy = abs (c1.Y - c2.Y)
        (dx = 0 && dy = 1) || (dx = 1 && dy = 0)
