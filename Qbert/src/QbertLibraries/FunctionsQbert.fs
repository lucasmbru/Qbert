namespace QbertFunctions

open System
open QbertLibraries.DataStructureQbert.Board // Import the DataStructure module
open QbertLibraries.DataStructureQbert.Player // Import the DataStructure module
open QbertLibraries.DataStructureQbert.Criatures // Import the DataStructure module


module FunctionsQbert = 


    module FunctionBoard = 

        // Function to print the board
        let printBoard (board : Board) =
            for row: Cell list in board do
                for cell: Cell in row do
                    printf "%c " (cell |> cellToChar)
                printfn ""

        // Function to get the character from a position in the board
        let getCharFromPosition (board : Board) (x : int, y : int) : char =
            // Aqui deberia hacerse una excepcion si x o y son mayores a la longitud de la lista
            // TODO
            List.nth (List.nth board x) y |> cellToChar
        
        let getCellFromPosition (board : Board) (x : int, y : int) : Cell =
            List.nth (List.nth board x) y

        let isBaseOfPyramid (board : Board) (x : int, y : int) =
            // Return true if the position is at the base of the pyramid, that is if x + y = list.length - 1
            let rows: int = List.length board
            if x + y = rows - 1 then true
            else false 

        // Function to update the board with the player's new position
        let updateBoard (board : Board) (player : Player) (previusPosition : Player) : Board * Player= 
            
            // Mark the previous position with Visited if the previus cell is NoVisited
            let boardWithHistory: Cell list list = 
                board |> List.mapi (fun (i: int) (row: Cell list) -> 
                    row |> List.mapi (fun (j: int) (cell: Cell) -> 
                        if i = previusPosition.X && j = previusPosition.Y && cell <> Empty then
                            match cell with  
                            | NoVisited -> Visited          // If the cell is NoVisited, we change it to Visited
                            | _ -> cell                     // If the cell is Visited or FlyingDics we don't change it
                        else cell)) 
                    
            // If the player is in a FlyingDisc, we must change that cell to Empty and move the player to the position (1, 1)
            let (newBoard: Cell list list, newPlayer: Player) = 
                if (getCellFromPosition boardWithHistory (player.X, player.Y) = FlyingDisc) then
                    // Mark the position (1, 1) as Visited if the player is in a FlyingDisc and move the player to (1, 1)
                    boardWithHistory |> List.mapi (fun (i: int) (row: Cell list) -> 
                        row |> List.mapi (fun (j: int) (cell: Cell) -> 
                            match (i, j) with
                            | 1, 1 -> Visited                                                                   // Mark the position (1, 1) as Visited
                            | pX, pY when pX = player.X && pY = player.Y -> Empty                    // Mark the position if FlyingDisc as Empty
                            | _ -> cell)), { player with X = 1; Y = 1 }                                         // Mark the position of the player to (1, 1)
                else 
                    // If the player is not in a FlyingDisc, we don't change the board
                    (boardWithHistory, player)
            (newBoard, newPlayer)


    module FunctionPlayer =

        // Function to move the player
        let movePlayer (player : Player) (dx : int, dy : int) = 
            {player with X = player.X + dx; Y = player.Y + dy}

        // Function to check if the move is valid matching with initialBoard
        let isValidMove (initialBoard: Board) (player : Player) =
            let cellPosition: Cell = FunctionBoard.getCellFromPosition initialBoard (player.X, player.Y)
            if cellPosition = Empty then false
            else true


        // Function to try moving the player and update the board if the move is valid
        let tryMovePlayer (initialBoard: Board) (board: Board) (player : Player) (dx : int, dy : int) =
            let newPlayer: Player  = movePlayer player (dx, dy)
            if isValidMove initialBoard newPlayer then
                let (newBoard: Cell list list, newPlayerCheck: Player) = FunctionBoard.updateBoard board newPlayer player
                (newBoard, newPlayerCheck)
            else
                // Decrease the lives if the move is invalid (QBert falls off the pyramid)
                let updatedPlayer: Player = { player with Lives = player.Lives - 1 }
                (initialBoard, updatedPlayer)

        // Function to control QBert's movement based on user input
        let controlPlayer (initialBoard: Board) (board: Board) player input =
            let (dx: int), (dy: int) =
                match input with
                | 'w' -> -1, 0  // Up
                | 's' -> 1, 0   // Down
                | 'a' -> 0, -1  // Left
                | 'd' -> 0, 1   // Right
                | _ -> 0, 0     // No move
            tryMovePlayer initialBoard board player (dx, dy)


    module FunctionCriatures = 
        
        let moveRedBall (initialBoard : Board) (board : Board) (redBall : RedBall) : RedBall =
            // If the red ball is active, it will move randomly
            let (dx: int), (dy: int) = 
                match Random().Next(0, 2) with
                | 0 -> 1, 0  //Down
                | 1 -> 0, 1  //Right
                | _ -> 0, 0  //No move

            let newX: int = redBall.X + dx
            let newY: int = redBall.Y + dy

            // We must check if the red ball fall off the pyramid and it becomes inactive
            let actualCharInBoard: char = FunctionBoard.getCharFromPosition board (newX, newY)
            if actualCharInBoard = 'A' then
                { redBall with X = newX; Y = newY }
            else
                { redBall with state_active = false }


        let movePurpleBall (initialBoard : Board) (board : Board) (player: Player) (purpleBall : PurpleBall) : PurpleBall =
            if purpleBall.is_snake then
                // If the purple ball is a snake, it will move in the way that minimizes the distance to the player
                let (dx: int), (dy: int) = player.X - purpleBall.X, player.Y - purpleBall.Y
                if Math.Abs(dx) > Math.Abs(dy) then
                    // In this case, move in the x direction, depending on the sign of dx
                    if dx > 0 then
                        let newX: int = purpleBall.X + 1
                        {purpleBall with X = newX} 
                    else 
                        let newX: int = purpleBall.X - 1
                        {purpleBall with X = newX}

                else if Math.Abs(dx) < Math.Abs(dy) then
                    // This other case, move in the y direction, depending on the sign of dy
                    if dy > 0 then
                        let newY: int = purpleBall.Y + 1
                        {purpleBall with Y = newY}
                    else
                        let newY: int = purpleBall.Y - 1
                        {purpleBall with Y = newY}

                    // From here, the distance to the player is the same in both directions, greater than 1 (otherwise, the snake would have reached the player)
                else if FunctionBoard.isBaseOfPyramid board (purpleBall.X, purpleBall.Y) then
                    // The snake is in the base of pyramid, there is two chances:
                    if FunctionBoard.isBaseOfPyramid board (player.X, player.Y) then    
                        // 1. The player is also in the base of the pyramid, so the snake will move in order to not fall off the pyramid 
                        // In this case, dx and dy have different signs. The snake will move along the axis which diferential is negative
                        if dx < 0 then {purpleBall with X = purpleBall.X - 1}
                        else {purpleBall with Y = purpleBall.Y - 1}
                    else 
                        // 2. The player is not in the base of the pyramid, so the snake will move randomly, decrasing the X or Y position
                        let idx: int = Random().Next(0, 2)
                        match idx with  
                        | 0 -> {purpleBall with X = purpleBall.X - 1}
                        | 1 -> {purpleBall with Y = purpleBall.Y - 1}
                        | _ -> purpleBall
                else
                    // The snake is not in the base of the pyramid and the distance to the player is the same in both directions
                    // In this case, the snake will move randomly, in order to reach the player
                    let idx: int = Random().Next(0, 2) //Random number between 0 and 1
                    match idx with
                    | 0 -> if dx > 0 then {purpleBall with X = purpleBall.X + 1} else {purpleBall with X = purpleBall.X - 1}
                    | 1 -> if dy > 0 then {purpleBall with Y = purpleBall.Y + 1} else {purpleBall with Y = purpleBall.Y - 1}
                    | _ -> purpleBall


            else // If the purple ball is not a snake yet
                let (dx: int), (dy: int) = 
                    match Random().Next(0, 2) with
                    | 0 -> 1, 0  //Down
                    | 1 -> 0, 1  //Right
                    | _ -> 0, 0  //No move

                let newX: int = purpleBall.X + dx
                let newY: int = purpleBall.Y + dy
                
                // We must check if the purple ball reaches the base of the pyramid and it converts into a snake
                let actualCharInBoard: char = FunctionBoard.getCharFromPosition board (newX, newY)
                if actualCharInBoard = 'A' then
                    if FunctionBoard.isBaseOfPyramid board (newX, newY) then
                        { purpleBall with X = newX; Y = newY; is_snake = true} // Active the snake if the purple ball reaches the base of the pyramid
                    else
                        { purpleBall with X = newX; Y = newY } 
                else purpleBall
        
