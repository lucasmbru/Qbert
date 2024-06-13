namespace QbertLibraries

open System

module DataStructureQbert = 

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
            let FlyinDiscTopInitial = { Y = nRandom1 }
            let FlyingDiscLeftInitial = { X = nRandom2 }
            (FlyinDiscTopInitial, FlyingDiscLeftInitial)


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

    module Player =

        // Define the type of the Position
        type Player = {
            X : int;
            Y : int;
            Lives : int;
        }

        // Initail player position at the top of the pyramid (1, 1) with a set number of lives
        let initialPlayer lives = { X = 1; Y = 1; Lives = lives}

    module Criatures = 

        // Define RedBall
        type RedBall = {
            X : int;
            Y : int;
            T_hop : int; // Time between hops
            state_active: bool; // True if the RedBall is active
        }

        // Initialize RedBall
        let initializeRedBall : RedBall = 
            let T_hop = 3
            let initialX: int = Random().Next(1, 3)
            if initialX = 1 then
                let initialY: int = 2
                { X = initialX; Y = initialY; T_hop = T_hop; state_active = true}
            else
                let initialY: int = 1
                { X = initialX; Y = initialY; T_hop = T_hop; state_active = true}

        //---------------------------------------------------------------------------------------//
        
        // Define PurpleBall
        type PurpleBall = {
            X : int;
            Y : int;
            T_hop : int; // Time between hops
            state_active: bool; // True if the PurpleBall is active
            is_snake: bool; // True if the PurpleBall is a snake
        }

        // Initialize PurpleBall
        let initializePurpleBall : PurpleBall = 
            let T_hop = 3 // This value could be replaced
            let initialX: int = Random().Next(1, 3)
            match initialX with
            | 1 -> { X = 1; Y = 2; T_hop = T_hop; state_active = true; is_snake = false}
            | 2 -> { X = 2; Y = 1; T_hop = T_hop; state_active = true; is_snake = false}
            | _ -> { X = 1; Y = 1; T_hop = T_hop; state_active = false; is_snake = false}   // This case is not possible

        //---------------------------------------------------------------------------------------//

        // Define Sam
        type Sam = {       
                X : int;
                Y : int;
                T_hop : int; // Time between hops
                state_active: bool; // True if Sam is active
        }

        // Initialize Sam
        let initializeSam : Sam = 
            let T_hop = 3 // This value could be replaced
            let initialX: int = Random().Next(1, 3)
            match initialX with
            | 1 -> { X = 1; Y = 2; T_hop = T_hop; state_active = true}
            | 2 -> { X = 2; Y = 1; T_hop = T_hop; state_active = true}
            | _ -> { X = 1; Y = 1; T_hop = T_hop; state_active = false}         // This case is not possible