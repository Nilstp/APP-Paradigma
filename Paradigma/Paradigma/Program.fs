open System

// Types
type Direction = Up | Down | Left | Right

type Cell = { X: int; Y: int; Walls: Set<Direction> }

type Maze = { width: int; height: int; cells: Cell list }

// Setup
let allWalls = [Up; Down; Left; Right]

let oppositeDirection dir =
    match dir with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

// Create maze with all walls intact
let createMaze width height =
    let cells =
        [ for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                { X = x; Y = y; Walls = Set.ofList allWalls } ]
    { width = width; height = height; cells = cells } 

// Get neighbors of a cell
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
    
// Remove wall
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

// Random generator
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

// Prim's algorithm
let primsAlgorithm maze =
    let startCell = maze.cells.[0]
    let mutable visited = Set.empty
    let mutable walls = []
    let mutable resultMaze = maze
    visited <- Set.add (startCell.X, startCell.Y) visited
    walls <- getNeighbors maze startCell
    while walls.Length > 0 do
        let (nx, ny, dir) = walls.[random.Next(walls.Length)]
        if not (Set.contains (nx, ny) visited) then
            resultMaze <- removeWall resultMaze { X = nx; Y = ny; Walls = Set.empty } dir
            visited <- Set.add (nx, ny) visited
            walls <- walls @ getNeighbors maze { X = nx; Y = ny; Walls = Set.empty }
        walls <- List.filter (fun (x, y, _) -> not (Set.contains (x, y) visited)) walls
    resultMaze


// ASCII Visualisation
let printMaze maze =
    let horizontalWall = "+---"
    let verticalWall = "|   "
    let corner = "+   "
    let empty = "    "
    for y in 0 .. maze.height - 1 do
        // Print upper walls
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells |> List.find (fun c -> c.X = x && c.Y = y)
            if Set.contains Up cell.Walls then
                printf "%s" horizontalWall
            else
                printf "%s" corner
        printfn "+"
        // Print vertical walls
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells |> List.find (fun c -> c.X = x && c.Y = y)
            if Set.contains Left cell.Walls then
                printf "%s" verticalWall
            else
                printf "%s" empty
        printfn "|"
        // Print bottom border
    for x in 0 .. maze.width - 1 do
        let cell = maze.cells |> List.find (fun c -> c.X = x && c.Y = maze.height - 1)
        if Set.contains Down cell.Walls then
            printf "%s" horizontalWall
        else
            printf "%s" corner
    printfn "+"

// Main
[<EntryPoint>]
let main argv =

    let maze = createMaze 10 10

    //let maze = randomConnect maze

    let maze = primsAlgorithm maze

    Console.SetWindowSize(800, 400);

    printMaze maze

    0