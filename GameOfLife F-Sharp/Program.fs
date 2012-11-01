open System
open System.Text
open System.Reactive.Linq
open System.Windows
open System.Windows.Controls
open FSharpx


let cellValue (board:int[,]) x y =
    if x >= 0 && y >= 0 && x < Array2D.length1 board && y < Array2D.length2 board then
        board.[x,y]
    else
        0

let liveNeighbours board x y =
    (cellValue board (x-1) (y-1)) +
    (cellValue board (x-1) (y)) +
    (cellValue board (x-1) (y+1)) +

    (cellValue board (x) (y-1)) +
    (cellValue board (x) (y+1)) +

    (cellValue board (x+1) (y-1)) +
    (cellValue board (x+1) (y)) +
    (cellValue board (x+1) (y+1))
    

let stepGame (oldBoard:int[,]) =
    Array2D.init (Array2D.length1 oldBoard) (Array2D.length2 oldBoard) (fun x y ->
        
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


type MainWindow = XAML<"Window.xaml">
let window = MainWindow()

let boardUpdates = new System.Reactive.Subjects.Subject<int[,]>()


let displayBoard (board:int[,]) = 
    boardUpdates.OnNext board

   
let rec runGame board = 
        displayBoard board
        System.Threading.Thread.Sleep(500)
        runGame (stepGame board)

[<STAThread>]
[<EntryPoint>]
let main argv =
    let board = Array2D.init 25 25 (fun x y ->
        match x, y with
            | 5, 5 -> 1
            | 5, 6 -> 1
            | 5, 7 -> 1
            | _,_ -> 0)

    window.startButton.Click.Add(fun _ -> 
        System.Threading.Tasks.Task.Factory.StartNew(fun x -> runGame board) |> ignore
        )

    boardUpdates.ObserveOn(window.Root).Subscribe(fun newBoard -> 
        let builder = new StringBuilder()
        Printf.bprintf builder "%A" newBoard
        let text = builder.ToString()
        window.textBox.Text <- text
        ) |> ignore
    
    let app = new System.Windows.Application()
    app.Run(window.Root) |> ignore

    0 




