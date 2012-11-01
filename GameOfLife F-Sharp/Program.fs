// Set up the board in code


let cellValue (board:int[,]) x y =
    if x >= 0 && y >= 0 && x < Array2D.length1 board && y < Array2D.length2 board then
        board.[x,y]
    else
        0

let liveNeighbours (board:int[,]) x y =
    (cellValue board (x-1) (y-1)) +
    (cellValue board (x-1) (y)) +
    (cellValue board (x-1) (y+1)) +

    (cellValue board (x) (y-1)) +
    (cellValue board (x) (y+1)) +

    (cellValue board (x+1) (y-1)) +
    (cellValue board (x+1) (y)) +
    (cellValue board (x+1) (y+1))
    


let stepGame (oldBoard:int[,]) =
    Array2D.init 24 24 (fun x y ->
        
        let neighbours = liveNeighbours oldBoard x y
        let alive = oldBoard.[x, y] = 1

        match alive, neighbours with
            // Any live cell with fewer than two live neighbours dies, as if caused by under-population.
            | true, x when x < 2 -> 0 
            // Any live cell with two or three live neighbours lives on to the next generation.
            | true, 2 -> 1
            | true, 3 -> 1
            // Any live cell with more than three live neighbours dies, as if by overcrowding.
            | true, x when x > 3 -> 0
            // Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
            | false, 3 -> 1

            // default is no change
            | _, _ -> oldBoard.[x, y]
        )

let rec runGame board =
        System.Console.Clear()
        board |> printfn "%A"
        System.Threading.Thread.Sleep(500)
        runGame (stepGame board)

[<EntryPoint>]
let main argv = 

    let board = Array2D.init 24 24 (fun x y ->
        match x, y with
            | 5, 5 -> 1
            | 5, 6 -> 1
            | 5, 7 -> 1
            | _,_ -> 0)

    runGame board

    0 
