namespace QbertFunctions

open System
open QbertLibraries.DataStructureQbert.Board // Import the DataStructure module Board
open QbertLibraries.DataStructureQbert.Player // Import the DataStructure module Player
open QbertLibraries.DataStructureQbert.Criatures // Import the DataStructure module Criatures
open QbertLibraries.DataStructureQbert.Score // Import the DataStructure module Score


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
        let updateBoard (board : Board) (player : Player) : Board * Player= 
            // Update the board with the player's new position and score 
                    
            // If the player is in a FlyingDisc, we must change that cell to Empty and move the player to the position (1, 1)

            if getCellFromPosition board (player.X, player.Y) = NoVisited then
                // If the player is in a NoVisited cell, we must change the cell to Visited cell and add scoreVisitNewCell to the player's score
                let newBoard: Cell list list  = 
                    board |> List.mapi (fun (i: int) (row: Cell list) -> 
                        row |> List.mapi (fun (j: int) (cell: Cell) -> 
                            if i = player.X && j = player.Y then
                                match cell with  
                                | NoVisited -> Visited          // If the cell is NoVisited, we change it to Visited
                                | _ -> cell                     // If the cell is Visited or FlyingDics we don't change it
                            else cell))
                // Return the updated board and the player with the new score
                (newBoard, { player with Score = player.Score + scoreVisitNewCell })
            else if (getCellFromPosition board (player.X, player.Y) = FlyingDisc) then
                // Mark the position (1, 1) as Visited if the player is in a FlyingDisc and move the player to (1, 1)
                board |> List.mapi (fun (i: int) (row: Cell list) -> 
                    row |> List.mapi (fun (j: int) (cell: Cell) -> 
                        match (i, j) with
                        | 1, 1 -> Visited                                                                    // Mark the position (1, 1) as Visited
                        | (pX: int), (pY: int) when pX = player.X && pY = player.Y -> Empty                  // Mark the position if FlyingDisc as Empty
                        | _ -> cell)), { player with X = 1; Y = 1 }                                          // Mark the position of the player to (1, 1)
            else 
                // If the player is not in a FlyingDisc, we don't change the board
                (board, player)


        let allCellsAreVisited (board: Board) =
            // Return true if all the cells in the pyramid are visited
            // We must check if in the board there are any NoVisited cell            

            let noNoVisited (cell: Cell) : bool = 
                // Return true if the cell is not NoVisited
                match cell with
                | NoVisited -> false
                | _ -> true
            
            board
            |> List.forall (fun (row: Cell list) -> row |> List.forall noNoVisited)
            

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
                let (newBoard: Cell list list, newPlayerCheck: Player) = FunctionBoard.updateBoard board newPlayer
                (newBoard, newPlayerCheck)
            else
                // Decrease the lives if the move is invalid (QBert falls off the pyramid)
                let updatedPlayer: Player = { player with X = 1; Y = 1; Lives = player.Lives - 1 }
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
        
        let moveRedBall (board : Board) (redBall : RedBall) : RedBall =
            // If the red ball is active, it will move randomly
            let (dx: int), (dy: int) = 
                match Random().Next(0, 2) with
                | 0 -> 1, 0  //Down
                | 1 -> 0, 1  //Right
                | _ -> 0, 0  //No move

            let newX: int = redBall.X + dx
            let newY: int = redBall.Y + dy

            // We must check if the red ball fall off the pyramid and it becomes inactive
            let actualCharInBoard: Cell = FunctionBoard.getCellFromPosition board (newX, newY)
            match actualCharInBoard with
            | Empty -> { redBall with state_active = false }
            | _ -> { redBall with X = newX; Y = newY }

        //---------------------------------------------------------------------------------------//

        let moveCoily (board: Board) (player: Player) (coily: Coily) : Coily =
            // Coily will move in the way that minimizes the distance to the player
            let (dx: int), (dy: int) = player.X - coily.X, player.Y - coily.Y
            if Math.Abs(dx) > Math.Abs(dy) then
                // In this case, move in the x direction, depending on the sign of dx
                if dx > 0 then
                    let newX: int = coily.X + 1
                    {coily with X = newX} 
                else 
                    let newX: int = coily.X - 1
                    {coily with X = newX}

            else if Math.Abs(dx) < Math.Abs(dy) then
                // This other case, move in the y direction, depending on the sign of dy
                if dy > 0 then
                    let newY: int = coily.Y + 1
                    {coily with Y = newY}
                else
                    let newY: int = coily.Y - 1
                    {coily with Y = newY}

            else
                // From here, the distance to the player is the same in both directions (in absolute value), greater than 1 (otherwise, Coily would have reached the player)
                
                if FunctionBoard.isBaseOfPyramid board (coily.X, coily.Y) then
                    // Coily is in the base of pyramid, there is two chances:
                    if FunctionBoard.isBaseOfPyramid board (player.X, player.Y) then    
                        // 1. The player is also in the base of the pyramid, so Coily will move in order to not fall off the pyramid 
                        // In this case, dx and dy have different signs. Coily will move along the axis which diferential is negative
                        if dx < 0 then 
                            {coily with X = coily.X - 1}
                        else 
                            {coily with Y = coily.Y - 1}
                    else 
                        // 2. The player is not in the base of the pyramid, so Coily will move randomly, decrasing the X or Y position
                        match Random().Next(0, 2) with  
                        | 0 -> {coily with X = coily.X - 1}
                        | 1 -> {coily with Y = coily.Y - 1}
                        | _ -> coily
                else
                    // Coily is not in the base of the pyramid and the distance to the player is the same in both directions
                    // In this case, Coily will move randomly, in order to reach the player
                    match Random().Next(0, 2) with
                    | 0 -> if dx > 0 then {coily with X = coily.X + 1} else {coily with X = coily.X - 1}
                    | 1 -> if dy > 0 then {coily with Y = coily.Y + 1} else {coily with Y = coily.Y - 1}
                    | _ -> coily

        //---------------------------------------------------------------------------------------//

        let movePurpleBall (board : Board) (purpleBall : PurpleBall) (player: Player) : PurpleBall =
            if purpleBall.is_snake then
                // Return a PurpleBall with a snake initialized in the position of the PurpleBall and state_active = false
                let coily: Coily = { purpleBall.coily with X = purpleBall.X; Y = purpleBall.Y }
                { purpleBall with state_active = false; coily = coily |> moveCoily board player }

            else // If the purple ball is not a snake yet
                let (dx: int), (dy: int) = 
                    match Random().Next(0, 2) with
                    | 0 -> 1, 0  //Down
                    | 1 -> 0, 1  //Right
                    | _ -> 0, 0  //No move

                let newX: int = purpleBall.X + dx
                let newY: int = purpleBall.Y + dy
                
                // We must check if the purple ball reaches the base of the pyramid and it converts into a snake
                let actualCharInBoard: Cell = FunctionBoard.getCellFromPosition board (newX, newY)
                match actualCharInBoard with
                | Visited | NoVisited -> if FunctionBoard.isBaseOfPyramid board (newX, newY) then
                                            { purpleBall with X = newX; Y = newY; state_active = false; is_snake = true} // Active the snake if the purple ball reaches the base of the pyramid
                                            else { purpleBall with X = newX; Y = newY }
                | _ -> purpleBall

        //---------------------------------------------------------------------------------------//

        let moveSam (board : Board) (sam : Sam) : Sam * Board = 
            // Sam will move randomly starting from the position (1, 2) or (2, 1)
            let (dx: int), (dy: int) = 
                match Random().Next(0, 2) with
                | 0 -> 1, 0  //Down
                | 1 -> 0, 1  //Right
                | _ -> 0, 0  //No move

            let newX: int = sam.X + dx
            let newY: int = sam.Y + dy

            // We musy check if Sam fall off the pyramid or, if Sam is in an Visited cell, change the cell to NoVisited
            let actualCharInBoard: Cell = FunctionBoard.getCellFromPosition board (newX, newY)
            match actualCharInBoard with
            | Empty -> ({sam with X = newX; Y = newY; state_active = false}, board)
            | Visited -> 
                let newBoard: Cell list list = 
                    board |> List.mapi (fun (i: int) (row: Cell list) -> 
                        row |> List.mapi (fun (j: int) (cell: Cell) -> 
                            if i = newX && j = newY then
                                NoVisited
                            else cell))
                ({sam with X = newX; Y = newY}, newBoard)
            | _ -> ({sam with X = newX; Y = newY}, board)

        //---------------------------------------------------------------------------------------//

        let moveGreenBall (board : Board) (greenBall : GreenBall) : GreenBall =
            // GreenBall will move randomly, starting from the position (1, 2) or (2, 1)
            let (dx: int), (dy: int) = 
                match Random().Next(0, 2) with
                | 0 -> 1, 0  //Down
                | 1 -> 0, 1  //Right
                | _ -> 0, 0  //No move
            
            let newX: int = greenBall.X + dx
            let newY: int = greenBall.Y + dy

            // We must check if the green ball fall off the pyramid
            let actualCellInBoard: Cell = FunctionBoard.getCellFromPosition board (newX, newY)
            match actualCellInBoard with
            | Empty -> {greenBall with state_active = false}
            | _ -> {greenBall with X = newX; Y = newY}
        
        //---------------------------------------------------------------------------------------//

        let moveUgg (ugg: Ugg) : Ugg =
            // Ugg will move randomly, starting from the bottom left corner of the board and stars moving up and to the right
            let (dx: int), (dy: int) = 
                match Random().Next(0, 2) with
                | 0 -> -1, 0  //Up
                | 1 -> -1, 1  //Diagonal
                | _ -> 0, 0   //No move
            
            let newX: int = ugg.X + dx
            let newY: int = ugg.Y + dy

            // Now, we must check if Ugg fall until row 1
            if newX = 1 then
                ({ugg with X = newX; Y = newY; state_active = false})
            else
                ({ugg with X = newX; Y = newY})

        //---------------------------------------------------------------------------------------//

        let moveWrongWay (wrongWay: WrongWay) : WrongWay =
            // WrongWay will move randomly, starting from the top right corner of the board and stars moving down and to the left 
            let (dx: int), (dy: int) = 
                match Random().Next(0, 2) with
                | 0 -> 0, -1  //Left
                | 1 -> 1, -1 //Diagonal
                | _ -> 0, 0  //No move
            let newX: int = wrongWay.X + dx
            let newY: int = wrongWay.Y + dy

            // Now, we must check if WrongWay fall until column 1
            if newY = 1 then
                ({wrongWay with X = newX; Y = newY; state_active = false})
            else
                ({wrongWay with X = newX; Y = newY})

        let moveCreature (creatures : Creatures) (board : Board) (player : Player) : Creatures * Board =
            match creatures with
            | RedBall (redBall: RedBall) -> (RedBall (moveRedBall board redBall), board)
            | PurpleBall (purpleBall: PurpleBall) -> (PurpleBall (movePurpleBall board purpleBall player), board)
            | Sam (sam: Sam) -> 
                let (newSam: Sam, newBoard: Board) = moveSam board sam
                (Sam newSam, newBoard)
            | GreenBall (greenBall: GreenBall) -> (GreenBall (moveGreenBall board greenBall), board)
            | Ugg (ugg: Ugg) -> (Ugg (moveUgg ugg), board)
            | WrongWay (wrongWay: WrongWay) -> (WrongWay (moveWrongWay wrongWay), board)


    module InteractionPlayerCriatures = 

        let checkPlayerRedBallCollision (board: Board) (player: Player) (redBall: RedBall) (creatures : Creatures list) : Board * Player * Creatures list * bool =
            if player.X = redBall.X && player.Y = redBall.Y then
                // If QBert collides with a RedBall, the player loses a life and stars in the same position, new board and the Creatures list empty and indicator of collision true
                let (flyingDT: FlyingDiscTop, flyingDL: FlyingDiscLeft) = initialiceFlyingDics 
                let initBoard = initialBoard flyingDT flyingDL
                (initBoard, { player with Lives = player.Lives - 1 }, [], true)
            else (board, player, creatures, false)

        //---------------------------------------------------------------------------------------//

        let checkPlayerCoilyCollision (board: Board) (player: Player) (coily: Coily) (creatures : Creatures list) : Board * Player * Creatures list * bool =
            if player.X = coily.X && player.Y = coily.Y then
                // If QBert collides with Coily, the player loses a life and stars in the top of the pyramid, new board and the Creatures list empty and indicator of collision true
                let (flyingDT: FlyingDiscTop, flyingDL: FlyingDiscLeft) = initialiceFlyingDics
                let initBoard: Board = initialBoard flyingDT flyingDL
                (initBoard, { player with X = 1; Y = 1; Lives = player.Lives - 1 }, [], true)
            else (board, player, creatures, false)

        //---------------------------------------------------------------------------------------//

        let checkPlayerPurpleBallCollision (board: Board) (player: Player) (purpleBall: PurpleBall) (creatures : Creatures list) : Board * Player * Creatures list * bool=
            if player.X = purpleBall.X && player.Y = purpleBall.Y && purpleBall.is_snake = false then
                // If QBert collides with a RedBall, the player loses a life and stars in the same position, new board and the Creatures list empty and indicator of collision true
                let (flyingDT: FlyingDiscTop, flyingDL: FlyingDiscLeft) = initialiceFlyingDics 
                let initBoard: Board = initialBoard flyingDT flyingDL
                (initBoard, { player with Lives = player.Lives - 1 }, [], true)
            else if purpleBall.is_snake then
                checkPlayerCoilyCollision board player purpleBall.coily creatures
            else (board, player, creatures, false)

        //---------------------------------------------------------------------------------------//

        let checkPlayerSamCollision (board: Board) (player: Player) (sam: Sam) (creatures : Creatures list) : Board * Player * Creatures list * bool=
            if player.X = sam.X && player.Y = sam.Y then
                // If QBert collides with Sam, player score will increase by some cuantity 
                let updatedPlayer: Player = { player with Score = player.Score + scoreMeetSam }
                // List filter to remove Sam from the list of creatures
                let creaturesWithoutSam = creatures |> List.filter (function Sam (s: Sam) when s.X = sam.X && s.Y = sam.Y -> false | _ -> true)
                // Return the updated board, player and creatures list and indicator of collision true
                (board, updatedPlayer, creaturesWithoutSam, true)
            else 
                // if there is no collision, return the same board, player and creatures list
                (board, player, creatures, false)

        //---------------------------------------------------------------------------------------//

        let checkPlayerGreenBallCollision (board: Board) (player: Player) (greenBall: GreenBall) (creatures : Creatures list) : Board * Player * Creatures list * bool =
            if player.X = greenBall.X && player.Y = greenBall.Y then
                // If QBert collides with a GreenBall, player score will increase by some cuantity and player has inmunization for some time
                let updatedPlayer: Player = { player with Score = player.Score + scoreMeetGreenBall; Inmunity = true }
                // List filter to remove GreenBall from the list of creatures
                let creaturesWithOutGreenBall : Creatures list = creatures |> List.filter (function GreenBall (g: GreenBall) when g.X = greenBall.X && g.Y = greenBall.Y -> false | _ -> true)
                // Return the updated board, player, creatures list and indicator of collision true 
                (board, updatedPlayer, creaturesWithOutGreenBall, true)
            else
                // if there is no collision, return the same board, player and creatures list
                (board, player, creatures, false)

        //---------------------------------------------------------------------------------------//

        let checkPlayerUggCollision (board: Board) (player: Player) (ugg: Ugg) (creatures : Creatures list) : Board * Player * Creatures list * bool =
            if player.X = ugg.X && player.Y = ugg.Y then
                // If QBert collides with Ugg, the player loses a life and stars in the same position, new board and the Creatures list empty and indicator of collision true
                let (flyingDT: FlyingDiscTop, flyingDL: FlyingDiscLeft) = initialiceFlyingDics
                let initBoard: Board = initialBoard flyingDT flyingDL
                (initBoard, { player with Lives = player.Lives - 1 }, [], true)                
            else 
                // if there is no collision, return the same board, player and creatures list
                (board, player, creatures, false)

        //---------------------------------------------------------------------------------------//

        let checkPlayerWrongWayCollision (board: Board) (player: Player) (wrongWay: WrongWay) (creatures: Creatures list) : Board * Player * Creatures list * bool =
            if player.X = wrongWay.X && player.Y = wrongWay.Y then
                // If QBert collides with WrongWay, the player loses a life and stars in the same position, new board and the Creatures list empty and indicator of collision true
                let (flyingDT: FlyingDiscTop, flyingDL: FlyingDiscLeft) = initialiceFlyingDics
                let initBoard: Board = initialBoard flyingDT flyingDL
                (initBoard, { player with Lives = player.Lives - 1 }, [], true)
            else
                // if there is no collision, return the same board, player and creatures list
                (board, player, creatures, false)

        //---------------------------------------------------------------------------------------//

        let checkPlayerCollision (board: Board) (player: Player) (creature: Creatures) (creaturesList: Creatures list) : Board * Player * Creatures list * bool =
            match creature with
            | RedBall (redBall: RedBall) -> checkPlayerRedBallCollision board player redBall creaturesList
            | PurpleBall (purpleBall: PurpleBall) -> checkPlayerPurpleBallCollision board player purpleBall creaturesList
            | Sam (sam: Sam) -> checkPlayerSamCollision board player sam creaturesList
            | GreenBall (greenBall: GreenBall) -> checkPlayerGreenBallCollision board player greenBall creaturesList
            | Ugg (ugg: Ugg) -> checkPlayerUggCollision board player ugg creaturesList
            | WrongWay (wrongWay: WrongWay) -> checkPlayerWrongWayCollision board player wrongWay creaturesList

        //---------------------------------------------------------------------------------------//

        let checkPlayerCollisionsForManyCreatures (board: Board) (player: Player) (creatures: Creatures list) : Board * Player * Creatures list =
            // This is a tough function to understand, you are warned...


            // Check if the player collides with any creature in the list
            // In case player has inmunity, the function will return the same board, player and creatures list
            if player.Inmunity then
                (board, player, creatures)
            else
            // In case of collision, the function will return the updated board, player and creatures list
            // In case of no collision, the function will return the same board, player and creatures list
                
                // Function to check the collisions recursively
                let rec checkCollisionsRec (board: Board, player: Player, creatures: Creatures list, collided: bool) (remainingCreatures: Creatures list) =
                    match remainingCreatures with
                   // Check if there is no more creatures in the list
                   // If there is no more creatures, return the updated board, player and creatures list
                    | [] -> (board, player, creatures)
                    // Check if there is more creatures in the list
                    // If there is more creatures, check the collision with the first creature in the list (head) 
                    | head :: tail ->
                        if collided then (board, player, creatures)
                        else
                            let (newBoard, newPlayer, newCreatures, hasCollided) = checkPlayerCollision board player head creatures
                            checkCollisionsRec (newBoard, newPlayer, newCreatures, hasCollided) tail
                // Call the function to check the collisions recursively initializating the indicator of collision in false and the list of creatures
                checkCollisionsRec (board, player, creatures, false) creatures