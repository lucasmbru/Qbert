namespace QbertTest

open System
open NUnit.Framework
open QbertLibraries.DataStructureQbert
open QbertFunctions.FunctionsQbert

[<TestFixture>]
type MyTests() =

    [<Test>]
    member this.``Test Example`` () =
        Assert.AreEqual(1, 1)

    [<Test>]
    member this.CoordinateChange() =
        let Coordinate1: Coordinate = {X=3; Y=1}
        let movement = MoveDirection.Up
        let actual = changeCoordinate Coordinate1 movement
        let expected: Coordinate = {X=2; Y=1}
        Assert.That(actual, Is.EqualTo(expected))


    [<Test>]
    member this.PyramidBaseTest() =
        let FDTop:Board.FlyingDiscTop = {Y=3}
        let FDLeft:Board.FlyingDiscLeft = {X=2}
        let Board1 = Board.initialBoard FDTop FDLeft
        let positionTest:Coordinate = {X=4; Y=1}
        let actual = FunctionBoard.isBaseOfPyramid Board1 positionTest
        let expected = false
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.UpdateBoardTestVisited() =
        let FDTop:Board.FlyingDiscTop = {Y=3}
        let FDLeft:Board.FlyingDiscLeft = {X=2}
        let Board1 = Board.initialBoard FDTop FDLeft
        let Player1:Player.Player = {
            Position = {X=1; Y=2};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        let actual = FunctionBoard.updateBoard Board1 Player1
        let Player2:Player.Player = {
            Position = {X=1; Y=2};
            Lives = 3;
            Score = 30;
            Inmunity = false;
        }
        let BoardSize = 9
        let Board2: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board2.[i].[j] <- Board.Cell.NoVisited
        Board2.[0].[3] <- Board.Cell.FlyingDisc
        Board2.[2].[0] <- Board.Cell.FlyingDisc
        Board2.[1].[2] <- Board.Cell.Visited
        let Board2List = Board2 |> Array.map Array.toList |> Array.toList
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.UpdateBoardTestFlyingDisc() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1.[i].[j] <- Board.Cell.NoVisited
        Board1.[0].[3] <- Board.Cell.FlyingDisc
        Board1.[2].[0] <- Board.Cell.FlyingDisc
        Board1.[1].[2] <- Board.Cell.Visited
        Board1.[1].[3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=0; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let actual = FunctionBoard.updateBoard Board1List Player1
        let Player2:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 3;
            Score = 90; // Se suma porque todavía no se había visitado la celda (1,1)
            Inmunity = false;
        } 
        let Board2: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board2.[i].[j] <- Board.Cell.NoVisited
        //Board2.[0].[3] <- Board.Cell.FlyingDisc // se elimina el flying disc
        Board2.[2].[0] <- Board.Cell.FlyingDisc
        Board2.[1].[2] <- Board.Cell.Visited
        Board1.[1].[3] <- Board.Cell.Visited
        Board2.[1].[1] <- Board.Cell.Visited
        let Board2List = Board2 |> Array.map Array.toList |> Array.toList
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))
        // este test falló porque no se consideró el aumento de puntaje en caso de que 
        // no se haya visitado previamente la celda (1,1)

    [<Test>]
    member this.AllCellsVisitedTest() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1.[i].[j] <- Board.Cell.Visited
        Board1.[0].[3] <- Board.Cell.FlyingDisc
        Board1.[2].[0] <- Board.Cell.FlyingDisc
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let actual = FunctionBoard.allCellsAreVisited Board1List
        let expected = true
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.MovePlayerValid() =
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let FDTop:Board.FlyingDiscTop = {Y=3}
        let FDLeft:Board.FlyingDiscLeft = {X=2}
        let BoardInitial = Board.initialBoard FDTop FDLeft
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1.[i].[j] <- Board.Cell.NoVisited
        Board1.[0].[3] <- Board.Cell.FlyingDisc
        Board1.[2].[0] <- Board.Cell.FlyingDisc
        Board1.[1].[2] <- Board.Cell.Visited
        Board1.[1].[3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let MovementDirection = MoveDirection.Down
        let actual = FunctionPlayer.tryMovePlayer BoardInitial Board1List Player1 MovementDirection
