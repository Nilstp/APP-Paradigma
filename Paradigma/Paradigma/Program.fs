open System

type Direction = Up | Down | Left | Right

type Cell = { X: int; Y: int; Walls: Set<Direction> }

type Maze = { width: int; height: int; cells: Cell list }


let allWalls = [Up; Down; Left; Right]

let oppositeDirection dir =
    match dir with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left


let createMaze width height =
    let cells =
        [ for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                { X = x; Y = y; Walls = Set.ofList allWalls } ]
    { width = width; height = height; cells = cells } 


// ASCII Visulaisatie

let printMaze maze =
    let horizontalWall = "+---"
    let verticalWall = "|   "
    let emptySpace = "    "
    for y in 0 .. maze.height - 1 do
        // Print horizontal walls
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells |> List.find (fun c -> c.X = x && c.Y = y)
            if Set.contains Up cell.Walls then
                printf "%s" horizontalWall
            else
                printf "%s" "+   "
        printfn "+"
        // Print vertical walls and spaces
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells |> List.find (fun c -> c.X = x && c.Y = y)
            if Set.contains Left cell.Walls then
                printf "%s" verticalWall
            else
                printf "%s" "    "
        printfn "|"


// Main
[<EntryPoint>]
let main argv =

    let maze = createMaze 30 30

    printMaze maze

    0