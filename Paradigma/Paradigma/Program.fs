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


let getNeighbors maze cell =
    let directions = [Up; Down; Left; Right]
    directions
    |> List.map (fun dir ->
        let (dx, dy) =
            match dir with
            | Up -> (0, -1)
            | Down -> (0, 1)
            | Left -> (-1, 0)
            | Right -> (1, 0)
        let neighborX = cell.X + dx
        let neighborY = cell.Y + dy
        if neighborX >= 0 && neighborX < maze.width && neighborY >= 0 && neighborY < maze.height then
            Some (neighborX, neighborY, dir)
        else
            None)
    |> List.choose id
    

let removeWall maze cell direction =
    let neighbor = getNeighbors maze cell |> List.tryFind (fun (_, _, dir) -> dir = direction)
    match neighbor with
    | Some (nx, ny, dir) ->
        let oppositeDir = oppositeDirection dir
        let updatedCells =
            maze.cells
            |> List.map (fun c ->
                if c.X = cell.X && c.Y = cell.Y then
                    { c with Walls = Set.remove dir c.Walls }
                elif c.X = nx && c.Y = ny then
                    { c with Walls = Set.remove oppositeDir c.Walls }
                else
                    c)
        { maze with cells = updatedCells }
    | None -> maze


let random = Random()

let randomConnect maze =
    maze.cells
    |> List.fold (fun accMaze cell ->
        let neighbors = getNeighbors accMaze cell
        if neighbors.Length > 0 then
            let (_, _, dir) = neighbors.[random.Next(neighbors.Length)]
            removeWall accMaze cell dir
        else
            accMaze
    ) maze


// ASCII Visualisatie
let printMaze maze =
    let horizontalWall = "+---"
    let verticalWall = "|   "
    let emptySpace = "    "
    for y in 0 .. maze.height - 1 do
        // Print bovenste muur
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells |> List.find (fun c -> c.X = x && c.Y = y)
            if Set.contains Up cell.Walls then
                printf "%s" horizontalWall
            else
                printf "%s" emptySpace
        printfn "+"
        // Print verticale muren en lege ruimte
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells |> List.find (fun c -> c.X = x && c.Y = y)
            if Set.contains Left cell.Walls then
                printf "%s" verticalWall
            else
                printf "    "
        printfn "|"


// Main
[<EntryPoint>]
let main argv =

    let maze = createMaze 30 30

    let maze = randomConnect maze

    printMaze maze

    0