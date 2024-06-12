namespace FuntionsQbert

open DataStructureQbert

module FunctionsQbert = 

    // Function to print the board
    let printBoard (board : Board) =
        for row in board do
            for cell in row do
                printf "%c " cell
            printfn ""

    // Function to move the player
    let movePlayer (player : Player) (dx : int, dy : int) = 
        {player with X = player.X + dx; Y = player.Y + dy}

    // Function to update the board with the player's new position
    let updateBoard (board : Board) (player : Player) previusPosition =
        // Mark the previous position with 'B'
        let boardWithHistory = 
            board |> List.mapi (fun i row -> 
                row |> List.mapi (fun j cell -> 
                    if i = previusPosition.X && j = previusPosition.Y then 'B' else cell))

        // Plave the player in the new position with 'Q'
        boardWithHistory |> List.mapi (fun i row -> 
            row |> List.mapi (fun j cell -> 
                if i = player.X && j = player.Y then 'Q' else cell))

    // Function to check if the move is valid matching with initialBoard
    let isValidMove (initialBoard: Board) player =
        let rows = List.length initialBoard
        let cols = List.length (List.head initialBoard)
        if player.X >= 0 && player.X < rows && player.Y >= 0 && player.Y < cols then
            match List.nth (List.nth initialBoard player.X) player.Y with
            | 'A' -> true
            | 'D' -> true
            | _ -> false
        else
            false

    // Function to try moving the player and update the board if the move is valid
    let tryMovePlayer (initialBoard: Board) (board: Board) (player : Player) (dx : int, dy : int) =
        let newPlayer  = movePlayer player (dx, dy)
        if isValidMove initialBoard newPlayer then
            let newBoard = updateBoard board newPlayer player
            Some(newBoard, newPlayer)
        else
            // Decrease the lives if the move is invalid (QBert falls off the pyramid)
            let updatedPlayer = { player with Lives = player.Lives - 1 }
            None, updatedPlayer


    // Function to control QBert's movement based on user input
    let controlPlayer (initialBoard: Board) (board: Board) player input =
        let dx, dy =
            match input with
            | 'w' -> -1, 0  // Up
            | 's' -> 1, 0   // Down
            | 'a' -> 0, -1  // Left
            | 'd' -> 0, 1   // Right
            | _ -> 0, 0     // No move
        tryMovePlayer initialBoard board player (dx, dy)