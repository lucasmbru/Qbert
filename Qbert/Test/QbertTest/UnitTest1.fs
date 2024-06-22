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
    member this.PlayerMoveUp() =
        let Player1: Player.Player = {
            Position = {X=3; Y=1};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        let movementDirection = MoveDirection.Up
        let actual = FunctionPlayer.movePlayer Player1 movementDirection
        let expected: Player.Player = {
            Position = {X=2; Y=1};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.PlayerMoveDown() =
        let Player1: Player.Player = {
            Position = {X=3; Y=1};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        let movementDirection = MoveDirection.Down
        let actual = FunctionPlayer.movePlayer Player1 movementDirection
        let expected: Player.Player = {
            Position = {X=4; Y=1};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.PlayerMoveLeft() =
        let Player1: Player.Player = {
            Position = {X=3; Y=2};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        let movementDirection = MoveDirection.Left
        let actual = FunctionPlayer.movePlayer Player1 movementDirection
        let expected: Player.Player = {
            Position = {X=3; Y=1};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.PlayerMoveRight() =
        let Player1: Player.Player = {
            Position = {X=3; Y=2};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        let movementDirection = MoveDirection.Right
        let actual = FunctionPlayer.movePlayer Player1 movementDirection
        let expected: Player.Player = {
            Position = {X=3; Y=3};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
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
                Board2[i][j] <- Board.Cell.NoVisited
        Board2[0][3] <- Board.Cell.FlyingDisc
        Board2[2][0] <- Board.Cell.FlyingDisc
        Board2[1][2] <- Board.Cell.Visited
        let Board2List = Board2 |> Array.map Array.toList |> Array.toList
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.UpdateBoardTestAlreadyVisited() =
        let BoardSize = 9
        let Board1: Board.Cell array array = 
            Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize do
            for j in 1..BoardSize-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 0;
            Inmunity = false;
        }
        let actual = FunctionBoard.updateBoard Board1List Player1
        let expected = (Board1List, Player1)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.UpdateBoardTestFlyingDisc() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
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
                Board2[i][j] <- Board.Cell.NoVisited
        Board2[2][0] <- Board.Cell.FlyingDisc
        Board2[1][2] <- Board.Cell.Visited
        Board2[1][3] <- Board.Cell.Visited
        Board2[1][1] <- Board.Cell.Visited
        let Board2List = Board2 |> Array.map Array.toList |> Array.toList
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.AllCellsVisitedTest() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.Visited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let actual = FunctionBoard.allCellsAreVisited Board1List
        let expected = true
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.MovePlayerValid() =
        let BoardSize = 9
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let MovementDirection = MoveDirection.Down
        let actual = FunctionPlayer.tryMovePlayer Board1List Player1 MovementDirection
        let Player2:Player.Player = {
            Position = {X=2; Y=3};
            Lives = 3;
            Score = 90;
            Inmunity = false;
        }
        let Board2: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board2[i][j] <- Board.Cell.NoVisited
        Board2[0][3] <- Board.Cell.FlyingDisc
        Board2[2][0] <- Board.Cell.FlyingDisc
        Board2[1][2] <- Board.Cell.Visited
        Board2[1][3] <- Board.Cell.Visited
        Board2[2][3] <- Board.Cell.Visited
        let Board2List = Board2 |> Array.map Array.toList |> Array.toList
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.MovePlayerInvalid() =
        let BoardSize = 9
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][4] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let MovementDirection = MoveDirection.Up
        let actual = FunctionPlayer.tryMovePlayer Board1List Player1 MovementDirection
        let Player2:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 2;
            Score = 60;
            Inmunity = false;
        }
        let expected = (Board1List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckPlayerRedBallCollision() =
        let BoardSize = 9
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let RedBall1:Creatures.RedBall = {
            Position = {X=1; Y=3}; 
            State_active = true
        }
        let PurpleBall1:Creatures.PurpleBall = {
            Position = {X=4; Y=1}; 
            State_active = true; 
            Is_snake = false; 
            Coily = {Position = {X=4; Y=1}; State_active = true}
        }
        let CreaturesList = [
            Creatures.RedBall RedBall1;
            Creatures.PurpleBall PurpleBall1
        ]

        let Player2:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 2;
            Score = 60;
            Inmunity = false;
        }

        let actual = InteractionPlayerCriatures.checkPlayerRedBallCollision Board1List Player1 RedBall1 CreaturesList
        let emptyList: Creatures.Creatures list = List.empty
        let expected = (Board1List, Player2, emptyList, true)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckPlayerRedBallCollisionFalse() =
        let BoardSize = 9
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let RedBall1:Creatures.RedBall = {
            Position = {X=1; Y=2}; 
            State_active = true
        }
        let PurpleBall1:Creatures.PurpleBall = {
            Position = {X=4; Y=1}; 
            State_active = true; 
            Is_snake = false; 
            Coily = {Position = {X=4; Y=1}; State_active = true}
        }
        let CreaturesList = [
            Creatures.RedBall RedBall1;
            Creatures.PurpleBall PurpleBall1
        ]

        let actual = InteractionPlayerCriatures.checkPlayerRedBallCollision Board1List Player1 RedBall1 CreaturesList
        let expected = (Board1List, Player1, CreaturesList, false)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckPurpleBallToSnakeTrue() =
        let BoardSize = 9
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let RedBall1:Creatures.RedBall = {
            Position = {X=1; Y=2}; 
            State_active = true
        }
        let PurpleBall1:Creatures.PurpleBall = {
            Position = {X=6; Y=1}; 
            State_active = true; 
            Is_snake = false; 
            Coily = {Position = {X=7; Y=1}; State_active = true}
        }
        let CreaturesList = [
            Creatures.RedBall RedBall1;
            Creatures.PurpleBall PurpleBall1
        ]
        let actual = FunctionCriatures.movePurpleBall Board1List PurpleBall1 Player1
        let PurpleBallDown:Creatures.PurpleBall = {
            Position = {X=7; Y=1}; 
            State_active = false; 
            Is_snake = true; 
            Coily = {Position = {X=7; Y=1}; State_active = true}
        }
        let purpleBallRigth:Creatures.PurpleBall = {
            Position = {X=6; Y=2}; 
            State_active = false; 
            Is_snake = true; 
            Coily = {Position = {X=6; Y=2}; State_active = true}
        }

        if PurpleBallDown.Position = actual.Position then 
            let expected = PurpleBallDown
            Assert.That(actual, Is.EqualTo(expected))
        else 
            let expected = purpleBallRigth
            Assert.That(actual, Is.EqualTo(expected))

        
        
    [<Test>]
    member this.CheckPlayerCoilyCollisionTrue() =
        let BoardSize = 9
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let RedBall1:Creatures.RedBall = {
            Position = {X=1; Y=3}; 
            State_active = true
        }
        let PurpleBall1:Creatures.PurpleBall = {
            Position = {X=1; Y=3}; 
            State_active = true; 
            Is_snake = true; 
            Coily = {Position = {X=1; Y=3}; State_active = true}
        }
        let CreaturesList = [
            Creatures.Creatures.RedBall RedBall1;
            Creatures.Creatures.PurpleBall PurpleBall1
        ]
        let Player2:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 2;
            Score = 60;
            Inmunity = false;
        }
        
        let CreaturesListEmpty:List<Creatures.Creatures> = List.empty

        let actual = InteractionPlayerCriatures.checkPlayerCoilyCollision Board1List Player1 PurpleBall1.Coily CreaturesList
        let expected = (Board1List, Player2, CreaturesListEmpty, true)
        Assert.That(actual, Is.EqualTo(expected))
    

    [<Test>]
    member this.CheckPlayerSamCollisionTrue() =
        let BoardSize = 9
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let RedBall1:Creatures.RedBall = {
            Position = {X=3; Y=2}; 
            State_active = true
        }
        let PurpleBall1:Creatures.PurpleBall = {
            Position = {X=5; Y=1}; 
            State_active = true; 
            Is_snake = true; 
            Coily = {Position = {X=1; Y=3}; State_active = true}
        }
        let Sam1:Creatures.Sam = {
            Position = {X=1; Y=3}; 
            State_active = true
        }
        let CreaturesList = [
            Creatures.Creatures.RedBall RedBall1;
            Creatures.Creatures.PurpleBall PurpleBall1;
            Creatures.Creatures.Sam Sam1
        ]
        let Player2:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 110;
            Inmunity = false;
        }
        
        let CreaturesListWithoutSam = 
            CreaturesList |> List.filter (fun x -> x <> Creatures.Creatures.Sam Sam1)

        let actual = InteractionPlayerCriatures.checkPlayerSamCollision Board1List Player1 Sam1 CreaturesList
        let expected = (Board1List, Player2, CreaturesListWithoutSam, true)
        Assert.That(actual, Is.EqualTo(expected))


    [<Test>]
    member this.CheckPlayerInputW() =
        let BoardSize = 9
        let initialBoard: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                initialBoard.[i].[j] <- Board.Cell.NoVisited
        initialBoard.[0].[3] <- Board.Cell.FlyingDisc
        initialBoard.[2].[0] <- Board.Cell.FlyingDisc
        let initialBoardList = initialBoard |> Array.map Array.toList |> Array.toList
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[2][1] <- Board.Cell.Visited
        Board1[3][1] <- Board.Cell.Visited
        Board1[3][2] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=3; Y=2};
            Lives = 3;
            Score = 90;
            Inmunity = false;
        }
        let input:char = 'w'
        let Player2:Player.Player = {
            Position = {X=2; Y=2};
            Lives = 3;
            Score = 120;
            Inmunity = false;
        }
        let NewBoard: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                NewBoard.[i].[j] <- Board.Cell.NoVisited
        NewBoard.[0].[3] <- Board.Cell.FlyingDisc
        NewBoard.[2].[0] <- Board.Cell.FlyingDisc
        NewBoard.[2].[1] <- Board.Cell.Visited
        NewBoard.[3].[1] <- Board.Cell.Visited
        NewBoard.[3].[2] <- Board.Cell.Visited
        NewBoard.[2].[2] <- Board.Cell.Visited
        let Board2List = NewBoard |> Array.map Array.toList |> Array.toList
        let actual = FunctionPlayer.controlPlayer Board1List Player1 input
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckPlayerInputA() =
        let BoardSize = 9
        let initialBoard: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                initialBoard.[i].[j] <- Board.Cell.NoVisited
        initialBoard.[0].[3] <- Board.Cell.FlyingDisc
        initialBoard.[2].[0] <- Board.Cell.FlyingDisc
        let initialBoardList = initialBoard |> Array.map Array.toList |> Array.toList
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[2][1] <- Board.Cell.Visited
        Board1[3][1] <- Board.Cell.Visited
        Board1[3][2] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=3; Y=2};
            Lives = 3;
            Score = 90;
            Inmunity = false;
        }
        let input:char = 'a'
        let Player2:Player.Player = {
            Position = {X=3; Y=1};
            Lives = 3;
            Score = 90;
            Inmunity = false;
        }
        let NewBoard: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                NewBoard.[i].[j] <- Board.Cell.NoVisited
        NewBoard.[0].[3] <- Board.Cell.FlyingDisc
        NewBoard.[2].[0] <- Board.Cell.FlyingDisc
        NewBoard.[2].[1] <- Board.Cell.Visited
        NewBoard.[3].[1] <- Board.Cell.Visited
        NewBoard.[3].[2] <- Board.Cell.Visited
        let Board2List = NewBoard |> Array.map Array.toList |> Array.toList
        let actual = FunctionPlayer.controlPlayer Board1List Player1 input
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckPlayerInputD() =
        let BoardSize = 9
        let initialBoard: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                initialBoard.[i].[j] <- Board.Cell.NoVisited
        initialBoard.[0].[3] <- Board.Cell.FlyingDisc
        initialBoard.[2].[0] <- Board.Cell.FlyingDisc
        let initialBoardList = initialBoard |> Array.map Array.toList |> Array.toList
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[2][1] <- Board.Cell.Visited
        Board1[3][1] <- Board.Cell.Visited
        Board1[3][2] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=3; Y=2};
            Lives = 3;
            Score = 90;
            Inmunity = false;
        }
        let input:char = 'd'
        let Player2:Player.Player = {
            Position = {X=3; Y=3};
            Lives = 3;
            Score = 120;
            Inmunity = false;
        }
        let NewBoard: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                NewBoard.[i].[j] <- Board.Cell.NoVisited
        NewBoard.[0].[3] <- Board.Cell.FlyingDisc
        NewBoard.[2].[0] <- Board.Cell.FlyingDisc
        NewBoard.[2].[1] <- Board.Cell.Visited
        NewBoard.[3].[1] <- Board.Cell.Visited
        NewBoard.[3].[2] <- Board.Cell.Visited
        NewBoard.[3].[3] <- Board.Cell.Visited
        let Board2List = NewBoard |> Array.map Array.toList |> Array.toList
        let actual = FunctionPlayer.controlPlayer Board1List Player1 input
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckPlayerInputS() =
        let BoardSize = 9
        let initialBoard: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                initialBoard.[i].[j] <- Board.Cell.NoVisited
        initialBoard.[0].[3] <- Board.Cell.FlyingDisc
        initialBoard.[2].[0] <- Board.Cell.FlyingDisc
        let initialBoardList = initialBoard |> Array.map Array.toList |> Array.toList
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[2][1] <- Board.Cell.Visited
        Board1[3][1] <- Board.Cell.Visited
        Board1[3][2] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=3; Y=2};
            Lives = 3;
            Score = 90;
            Inmunity = false;
        }
        let input:char = 's'
        let Player2:Player.Player = {
            Position = {X=4; Y=2};
            Lives = 3;
            Score = 120;
            Inmunity = false;
        }
        let NewBoard: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                NewBoard[i][j] <- Board.Cell.NoVisited
        NewBoard[0][3] <- Board.Cell.FlyingDisc
        NewBoard[2][0] <- Board.Cell.FlyingDisc
        NewBoard[2][1] <- Board.Cell.Visited
        NewBoard[3][1] <- Board.Cell.Visited
        NewBoard[3][2] <- Board.Cell.Visited
        NewBoard[4][2] <- Board.Cell.Visited
        let Board2List = NewBoard |> Array.map Array.toList |> Array.toList
        let actual = FunctionPlayer.controlPlayer Board1List Player1 input
        let expected = (Board2List, Player2)
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckCoilyMovementX() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Coily1:Creatures.Coily = {
            Position = {X=7; Y=1};
            State_active = true
        }
        let actual = FunctionCriatures.moveCoily Board1List Player1 Coily1
        let Coily2:Creatures.Coily = {
            Position = {X=6; Y=1};
            State_active = true
        }
        let expected = Coily2
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckCoilyMovementY() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=3};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let Coily1:Creatures.Coily = {
            Position = {X=1; Y=7};
            State_active = true
        }
        let actual = FunctionCriatures.moveCoily Board1List Player1 Coily1
        let Coily2:Creatures.Coily = {
            Position = {X=1; Y=6};
            State_active = true
        }
        let expected = Coily2
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckRedBallMovementValid() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        Board1[1][4] <- Board.Cell.Visited
        Board1[1][5] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=5};
            Lives = 3;
            Score = 120;
            Inmunity = false;
        }
        let RedBall1:Creatures.RedBall = {
            Position = {X=2; Y=2};
            State_active = true
        }
        let RedBall2:Creatures.RedBall = {
            Position = {X=3; Y=2};
            State_active = true
        }
        let RedBall3:Creatures.RedBall = {
            Position = {X=2; Y=3};
            State_active = true
        }
        let resultado = FunctionCriatures.moveRedBall Board1List RedBall1
        let actual = resultado=RedBall2 || resultado=RedBall3
        let expected = true
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckRedBallMovementFall() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        Board1[1][4] <- Board.Cell.Visited
        Board1[1][5] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=5};
            Lives = 3;
            Score = 120;
            Inmunity = false;
        }
        let RedBall1:Creatures.RedBall = {
            Position = {X=7; Y=1};
            State_active = true
        }
        let RedBall2:Creatures.RedBall = {
            Position = {X=7; Y=1};
            State_active = false
        }
        let actual = FunctionCriatures.moveRedBall Board1List RedBall1
        let expected = RedBall2
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckCoilyMovementRandom() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        Board1[1][4] <- Board.Cell.Visited
        Board1[1][5] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 3;
            Score = 120;
            Inmunity = false;
        }
        let Coily1:Creatures.Coily = {
            Position = {X=3; Y=3};
            State_active = true
        }
        let Coily2:Creatures.Coily = {
            Position = {X=3; Y=2};
            State_active = true
        }
        let Coily3:Creatures.Coily = {
            Position = {X=2; Y=3};
            State_active = true
        }
        let resultado = FunctionCriatures.moveCoily Board1List Player1 Coily1
        let actual = resultado=Coily2 || resultado=Coily3
        let expected = true
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckSamMovement() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        Board1[1][4] <- Board.Cell.Visited
        Board1[1][5] <- Board.Cell.Visited
        Board1[2][1] <- Board.Cell.Visited
        Board1[2][2] <- Board.Cell.Visited
        Board1[3][2] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 3;
            Score = 210;
            Inmunity = false;
        }
        let Sam1:Creatures.Sam = {
            Position = {X=3; Y=1};
            State_active = true
        }
        let Sam2:Creatures.Sam = {
            Position = {X=3; Y=2};
            State_active = true
        }
        let Sam3:Creatures.Sam = {
            Position = {X=4; Y=1};
            State_active = true
        }
        let Board2: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board2[i][j] <- Board.Cell.NoVisited
        Board2[0][3] <- Board.Cell.FlyingDisc
        Board2[2][0] <- Board.Cell.FlyingDisc
        Board2[1][2] <- Board.Cell.Visited
        Board2[1][3] <- Board.Cell.Visited
        Board2[1][4] <- Board.Cell.Visited
        Board2[1][5] <- Board.Cell.Visited
        Board2[2][1] <- Board.Cell.Visited
        Board2[2][2] <- Board.Cell.Visited
        let Board2List = Board2 |> Array.map Array.toList |> Array.toList

        let resultado = FunctionCriatures.moveSam Board1List Sam1
        let actual = resultado=(Sam2,Board2List) || resultado=(Sam3,Board1List)
        let expected = true
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckGreenBallMovement() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let GreenBall1:Creatures.GreenBall = {
            Position = {X=3; Y=1};
            State_active = true
        }
        let GreenBall2:Creatures.GreenBall = {
            Position = {X=3; Y=2};
            State_active = true
        }
        let GreenBall3:Creatures.GreenBall = {
            Position = {X=4; Y=1};
            State_active = true
        }
        let resultado = FunctionCriatures.moveGreenBall Board1List GreenBall1
        let actual = resultado=GreenBall2 || resultado=GreenBall3
        let expected = true
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckGreenBallMovementFall() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }
        let GreenBall1:Creatures.GreenBall = {
            Position = {X=7; Y=1};
            State_active = true
        }
        let GreenBall2:Creatures.GreenBall = {
            Position = {X=7; Y=1};
            State_active = false
        }
        let actual = FunctionCriatures.moveGreenBall Board1List GreenBall1
        let expected = GreenBall2
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckUggMovement() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }        
        let Ugg1:Creatures.Ugg = {
            Position = {X=3; Y=1};
            State_active = true
        } 
        let Ugg2:Creatures.Ugg = {
            Position = {X=2; Y=1};
            State_active = true
        }
        let Ugg3:Creatures.Ugg = {
            Position = {X=2; Y=2};
            State_active = true
        }
        let resultado = FunctionCriatures.moveUgg Ugg1
        let actual = resultado=Ugg2 || resultado=Ugg3
        let expected = true
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.CheckWrongWayMove() =
        let BoardSize = 9
        let Board1: Board.Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Board.Cell.Empty))
        for i in 1..BoardSize-1 do
            for j in 1..BoardSize-1-i do
                Board1[i][j] <- Board.Cell.NoVisited
        Board1[0][3] <- Board.Cell.FlyingDisc
        Board1[2][0] <- Board.Cell.FlyingDisc
        Board1[1][2] <- Board.Cell.Visited
        Board1[1][3] <- Board.Cell.Visited
        let Board1List = Board1 |> Array.map Array.toList |> Array.toList
        let Player1:Player.Player = {
            Position = {X=1; Y=1};
            Lives = 3;
            Score = 60;
            Inmunity = false;
        }        
        let WrongWay1:Creatures.WrongWay = {
            Position = {X=1; Y=7};
            State_active = true
        }
        let WrongWay2:Creatures.WrongWay = {
            Position = {X=1; Y=6};
            State_active = true
        }
        let WrongWay3:Creatures.WrongWay = {
            Position = {X=2; Y=6};
            State_active = true
        }
        let resultado = FunctionCriatures.moveWrongWay WrongWay1
        let actual = (resultado.Position={X=1; Y=6}) || (resultado.Position={X=2; Y=6})
        let expected = true
        Assert.That(actual, Is.EqualTo(expected))
