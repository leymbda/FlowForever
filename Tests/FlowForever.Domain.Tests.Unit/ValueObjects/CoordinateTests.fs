namespace FlowForever.Domain

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CoordinateTests () =
    [<TestMethod>]
    [<DataRow(-1, 0)>]
    [<DataRow(0, -1)>]
    [<DataRow(1, 0)>]
    [<DataRow(0, 1)>]
    member _.``isTouching - Returns true when touching`` (x: int, y: int) =
        // Arrange
        let c1 = { X = 0; Y = 0 }
        let c2 = { X = x; Y = y }

        // Act
        let actual = Coordinate.isTouching c1 c2

        // Assert
        Assert.IsTrue(actual)

    [<TestMethod>]
    [<DataRow(-1, -1)>]
    [<DataRow(1, -1)>]
    [<DataRow(1, 1)>]
    [<DataRow(-1, 1)>]
    member _.``isTouching - Returns false when not touching`` (x: int, y: int) =
        // Arrange
        let c1 = { X = 0; Y = 0 }
        let c2 = { X = x; Y = y }

        // Act
        let actual = Coordinate.isTouching c1 c2

        // Assert
        Assert.IsFalse(actual)

    [<TestMethod>]
    member _.``isTouching - Returns false when same coordinate`` () =
        // Arrange
        let c1 = { X = 0; Y = 0 }
        let c2 = { X = 0; Y = 0 }

        // Act
        let actual = Coordinate.isTouching c1 c2

        // Assert
        Assert.IsFalse(actual)
