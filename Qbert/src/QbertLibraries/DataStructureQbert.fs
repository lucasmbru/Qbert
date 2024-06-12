namespace QbertLibraries

open System

module DataStructureQbert = 

    module Board = 

        // Define the type of the Board
        type Board = list<list<char>>

        // Define the initial state of the board
        let initialBoard : Board =
            [
                ['X'; 'D'; 'D'; 'D'; 'D'; 'D'; 'D'; 'D'; 'X']
                ['D'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'X']
                ['D'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'X'; 'X']
                ['D'; 'A'; 'A'; 'A'; 'A'; 'A'; 'X'; 'X'; 'X']
                ['D'; 'A'; 'A'; 'A'; 'A'; 'X'; 'X'; 'X'; 'X']
                ['D'; 'A'; 'A'; 'A'; 'X'; 'X'; 'X'; 'X'; 'X']
                ['D'; 'A'; 'A'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X']
                ['D'; 'A'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X']
                ['X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X']
            ]

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
            let T_hop = 3
            let initialX: int = Random().Next(1, 3)
            if initialX = 1 then
                let initialY: int = 2
                { X = initialX; Y = initialY; T_hop = T_hop; state_active = true; is_snake = false}
            else
                let initialY: int = 1
                { X = initialX; Y = initialY; T_hop = T_hop; state_active = true; is_snake = false}
