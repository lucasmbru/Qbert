namespace QbertLibraries

open System

module DataStructureQbert = 

    // Define the type Position
    type Coordinate = {
        X : int;            // X coordinate
        Y : int;            // Y coordinate
    }

    type MoveDirection = 
        | Up 
        | Down
        | Left
        | Right
        | UpLeft
        | UpRight
        | DownLeft
        | DownRight
        | NoMove

    let changeCoordinate (coordinate: Coordinate) (moveDirection: MoveDirection) : Coordinate = 
        match moveDirection with
        | Up -> { X = coordinate.X - 1; Y = coordinate.Y }
        | Down -> { X = coordinate.X + 1; Y = coordinate.Y }
        | Left -> { X = coordinate.X; Y = coordinate.Y - 1 }
        | Right -> { X = coordinate.X; Y = coordinate.Y + 1 }
        | UpLeft -> { X = coordinate.X - 1; Y = coordinate.Y - 1 }
        | UpRight -> { X = coordinate.X - 1; Y = coordinate.Y + 1 }
        | DownLeft -> { X = coordinate.X + 1; Y = coordinate.Y - 1 }
        | DownRight -> { X = coordinate.X + 1; Y = coordinate.Y + 1 }
        | NoMove -> { X = coordinate.X; Y = coordinate.Y }


    module Board = 

        // Define the size of the Board
        let BoardSize = 9

        // Define type of cell board
        type Cell = 
            | Empty
            | NoVisited
            | Visited
            | FlyingDisc

        // Matching char with Cell
        let cellToChar = function
            | Empty -> 'X'
            | NoVisited -> 'A'
            | Visited -> 'B'
            | FlyingDisc -> 'D'


        // Define the type of the Board
        type Board = list<list<Cell>>

        // Define FlyinDisc
        type FlyingDiscTop = {
            Y : int;
        }

        type FlyingDiscLeft = {
            X : int;
        }

        let initialiceFlyingDics : FlyingDiscTop * FlyingDiscLeft = 
            // Initialize the FlyingDiscs in random positions
            let nRandom1: int = Random().Next(1, BoardSize)
            let nRandom2: int = Random().Next(1, BoardSize)
            let flyinDiscTopInitial = { Y = nRandom1 }
            let flyingDiscLeftInitial = { X = nRandom2 }
            (flyinDiscTopInitial, flyingDiscLeftInitial)


        // Define the initial state of the board
        let initialBoard (flyingDiscTop: FlyingDiscTop) (flyingDiscLeft: FlyingDiscLeft) : Board =

            // We must initialize the board with the position of the FlyingDics:
            // For example, if the FlyingDiscTop is in the position (0, 4) and the 
            // FlyingDiscLeft is in the position (6, 0), the board will be:
            //[
            //    ['X'; 'X'; 'X'; 'X'; 'D'; 'X'; 'X'; 'X'; 'X']
            //    ['X'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'X']
            //    ['X'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'X'; 'X']
            //    ['X'; 'A'; 'A'; 'A'; 'A'; 'A'; 'X'; 'X'; 'X']
            //    ['X'; 'A'; 'A'; 'A'; 'A'; 'X'; 'X'; 'X'; 'X']
            //    ['X'; 'A'; 'A'; 'A'; 'X'; 'X'; 'X'; 'X'; 'X']
            //    ['D'; 'A'; 'A'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X']
            //    ['X'; 'A'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X']
            //    ['X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X']
            //]


            // Initialize the board with 'X'
            let board: Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Empty))

            // Set the NoVisited spaces
            for i in 1..BoardSize-1 do
                for j in 1..BoardSize-1-i do
                    board.[i].[j] <- NoVisited

            // Set the FlyingDiscs
            board.[0].[flyingDiscTop.Y] <- FlyingDisc
            board.[flyingDiscLeft.X].[0] <- FlyingDisc

            // Convert the board from array to list of lists
            let boardList = board |> Array.map Array.toList |> Array.toList

            boardList

        // Define the finish state of the board
        let finishBoard : Board = 
            // The finish board is the same as the initial board but with all the cells visited
            let board: Cell array array = Array.init BoardSize (fun _ -> Array.init BoardSize (fun _ -> Empty))

            // Set the NoVisited spaces
            for i in 1..BoardSize-1 do
                for j in 1..BoardSize-1-i do
                    board.[i].[j] <- Visited

            // Convert the board from array to list of lists
            let boardList: Cell list list = board |> Array.map Array.toList |> Array.toList

            boardList
            


    module Player =

        // Define the type of the Position
        let lives: int = 4

        // Define the time of the hop of the player
        let tHopPlayer = 3

        // Define the type of the Position
        type Player = {
            Position : Coordinate;
            Lives : int;
            Score : int;
            Inmunity : bool;
        }

    module Creatures = 

        // Define the time of the hop of the criatures
        let tHopCreature = 3

        // Define RedBall
        type RedBall = {
            Position : Coordinate;    // Position of the RedBall
            State_active: bool;         // True if the RedBall is active
        }

        // Initialize RedBall
        let initializeRedBall : RedBall = 
            match Random().Next(0, 2) with
            | 0 -> { Position = { X = 1; Y = 2 }; State_active = true}
            | 1 -> { Position = { X = 2; Y = 1}; State_active = true}
            | _ -> { Position = { X = 1; Y = 1}; State_active = false}         // This case is not possible

        //---------------------------------------------------------------------------------------//
        
        // Define Coily
        type Coily = {
            Position : Coordinate; // Position of Coily
            State_active: bool; // True if Coily is active
        }

        // Initialize Coily
        let initializeCoily (position: Coordinate) : Coily = 
            // Coily the snake starts with the position given
            { Position = position; State_active = true}
        //---------------------------------------------------------------------------------------//

        // Define PurpleBall
        type PurpleBall = {
            Position : Coordinate;  // Position of the PurpleBall
            State_active: bool;     // True if the PurpleBall is active
            Is_snake: bool;         // True if the PurpleBall is becomes into a snake
            Coily: Coily;           // Coily of the PurpleBall
        }

        // Initialize PurpleBall
        let initializePurpleBall : PurpleBall = 
            match Random().Next(0, 2) with
            | 0 -> { Position = { X = 1; Y = 2 }; State_active = true; Is_snake = false; Coily = initializeCoily { X = 1; Y = 2}}
            | 1 -> { Position = { X = 2; Y = 1}; State_active = true; Is_snake = false; Coily = initializeCoily { X = 2; Y = 1}}
            | _ -> { Position = { X = 1; Y = 1}; State_active = false; Is_snake = false; Coily = initializeCoily { X = 1; Y = 1}}         // This case is not possible
        //---------------------------------------------------------------------------------------//

        // Define Sam
        type Sam = {       
            Position: Coordinate; // Position of Sam
            State_active: bool; // True if Sam is active
        }

        // Initialize Sam
        let initializeSam : Sam = 
            match Random().Next(0, 2) with
            | 0 -> { Position = { X = 1; Y = 2 }; State_active = true}
            | 1 -> { Position = { X = 2; Y = 1}; State_active = true}
            | _ -> { Position = { X = 1; Y = 1}; State_active = false}         // This case is not possible

        //---------------------------------------------------------------------------------------//

        // Define GreenBall
        type GreenBall = {
            Position: Coordinate; // Position of the GreenBall
            State_active: bool; // True if the GreenBall is active
        }

        // Initialize GreenBall
        let initializeGreenBall : GreenBall = 
            match Random().Next(0, 2) with
            | 0 -> { Position = { X = 1; Y = 2 }; State_active = true}
            | 1 -> { Position = { X = 2; Y = 1}; State_active = true}
            | _ -> { Position = { X = 1; Y = 1}; State_active = false}         // This case is not possible
        //---------------------------------------------------------------------------------------//

        // Define Ugg
        type Ugg = {
            Position: Coordinate; // Position of Ugg
            State_active: bool; // True if Ugg is active
        }

        // Initialize Ugg
        let initializeUgg : Ugg = 
            let (initialX: int, initialY: int) = (Board.BoardSize-1, 1)         // Ugg starts at the bottom left corner
            { Position = { X = initialX; Y = initialY}; State_active = true}

        //---------------------------------------------------------------------------------------//

        // Define WrongWay
        type WrongWay = {
            Position: Coordinate; // Position of WrongWay
            State_active: bool; // True if WrongWay is active
        }

        // Initialize WrongWay
        let initializeWrongWay : WrongWay = 
            let (initialX: int, initialY: int) = (1, Board.BoardSize-1)         // WrongWay starts at the top right corner
            { Position = { X = initialX; Y = initialY}; State_active = true}

        //---------------------------------------------------------------------------------------//

        // Define a date type for the all criatures
        type Creatures = 
            | RedBall of RedBall
            | PurpleBall of PurpleBall
            | Sam of Sam
            | GreenBall of GreenBall
            | Ugg of Ugg
            | WrongWay of WrongWay

    module Score = 

        let scoreVisitNewCell: int = 30
        let scoreMeetSam: int = 50
        let scoreMeetGreenBall: int = 100