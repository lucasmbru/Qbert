namespace QbertLibraries

module DataStructureQbert = 

    // Define the type of the Board
    type Board = list <list <char>>

    // Define the type of the Position
    type Player = {
        X : int;
        Y : int;
        Lives : int;
    }

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

    // Initail player position at the top of the pyramid (1, 1) with a set number of lives
    let initialPlayer lives = { X = 1; Y = 1; Lives = lives}