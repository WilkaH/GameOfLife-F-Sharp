﻿open System
open System.Text
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Reactive.Linq
open System.Windows
open System.Windows.Controls
open FSharpx

let BoardWidth = 150
let BoardHeight = 150
let CellSize = 10


let boardUpdates = new System.Reactive.Subjects.Subject<ImageSource>()
    
let rec MakeCellRowImpl bytes n =
    match n with
    | 1 -> bytes
    | _ -> List.append bytes (MakeCellRowImpl bytes (n-1))   


let MakeCellRow (bytes:List<byte>) =
    (MakeCellRowImpl bytes CellSize) |> List.toArray


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
    

let nextBoardImage (oldBoard:int[,]) (nextBoard:int[,]) oldImage = 

    // B G R
    let liveRow = MakeCellRow ([0uy; 0uy; 0uy; 0uy])
    let deadRow = MakeCellRow ([64uy; 0uy; 250uy; 0uy]) 
    
    let image = new WriteableBitmap(oldImage)
    for x = 0 to (Array2D.length1 oldBoard) - 1  do
        for y = 0 to (Array2D.length2 oldBoard) - 1  do
                        
            if oldBoard.[x,y] <> nextBoard.[x,y] then
                let imageX = x * CellSize
                let imageY = y * CellSize
            
                let pixelColor = if nextBoard.[x,y] = 1 then liveRow else deadRow

                for innerY = 0 to CellSize - 1 do
                    let rect = new System.Windows.Int32Rect(imageX, (imageY + innerY), CellSize, 1)
                    image.WritePixels(rect, pixelColor, (pixelColor.Length), 0)
            
    image.Freeze()
    image

let initBoardImage (board:int[,]) = 
    let invalidBoard = Array2D.init BoardWidth BoardHeight (fun x y -> -1)
    let blankImage = new WriteableBitmap(CellSize * BoardWidth, CellSize * BoardHeight, 96.0, 96.0, PixelFormats.Bgr32, null)
    nextBoardImage invalidBoard board blankImage

let stepGame (oldBoard:int[,]) oldImage =
    let nextBoard = Array2D.init (Array2D.length1 oldBoard) (Array2D.length2 oldBoard) (fun x y ->        
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

    let nextImage = nextBoardImage oldBoard nextBoard oldImage
    (nextBoard, nextImage)


type MainWindow = XAML<"Window.xaml">
let window = MainWindow()

let rec runGame board boardImage = 
        boardUpdates.OnNext boardImage
        System.Threading.Thread.Sleep(50)

        let (nextBoard, nextImage) = stepGame board boardImage 

        runGame nextBoard nextImage
    
let initGame board = 
    let boardImage = initBoardImage board
    boardUpdates.OnNext boardImage
    System.Threading.Thread.Sleep(50)
    runGame board boardImage

[<STAThread>]
[<EntryPoint>]
let main argv =
    let board = Array2D.init BoardWidth BoardHeight (fun x y ->
        match x, y with
            // Blinker 
            | 5, 5 -> 1
            | 5, 6 -> 1
            | 5, 7 -> 1

            // Toad 
            | 10, 8 -> 1
            | 11, 8 -> 1
            | 12, 8 -> 1
            | 11, 9 -> 1
            | 12, 9 -> 1
            | 13, 9 -> 1

            // Acorn
            | 83, 85 -> 1
            | 82, 87 -> 1
            | 83, 87 -> 1
            | 85, 86 -> 1
            | 86, 87 -> 1
            | 87, 87 -> 1
            | 88, 87 -> 1
            
            // default
            | _,_ -> 0)

    window.startButton.Click.Add(fun _ -> 
        System.Threading.Tasks.Task.Factory.StartNew(fun x -> initGame board) |> ignore
        )

    boardUpdates.ObserveOn(window.Root).Subscribe(fun boardImage -> 
        window.image.Source <- boardImage
        
        ) |> ignore
    
    let app = new System.Windows.Application()
    app.Run(window.Root) 





