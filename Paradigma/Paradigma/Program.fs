open System

// Types
type Direction = Up | Down | Left | Right

type Cell = { X: int; Y: int; Walls: Set<Direction> }

type Wall = Cell * Direction

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

// Get cell from maze
let getCell maze x y =
    maze.cells |> List.find (fun c -> c.X = x && c.Y = y)

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
            Some (getCell maze neighborX neighborY, dir)
        else
            None)
    |> List.choose id
    
// Remove wall between two cells
let removeWall maze cell direction =
    let neighbors = getNeighbors maze cell
    match neighbors |> List.tryFind (fun (_, dir) -> dir = direction) with
    | Some (neighbor, dir) ->
        let oppositeDir = oppositeDirection dir
        let updatedCells =
            maze.cells
            |> List.map (fun c ->
                if c.X = cell.X && c.Y = cell.Y then
                    { c with Walls = Set.remove dir c.Walls }
                elif c.X = neighbor.X && c.Y = neighbor.Y then
                    { c with Walls = Set.remove oppositeDir c.Walls }
                else
                    c)
        { maze with cells = updatedCells }
    | None -> maze

// Remove outer wall of a cell (used for entrance and exit)
let removeOuterWall maze x y direction =
    let updatedCells =
        maze.cells
        |> List.map (fun c ->
            if c.X = x && c.Y = y then
                { c with Walls = Set.remove direction c.Walls }
            else
                c)
    { maze with cells = updatedCells }

// Prim's algorithm
let primsAlgorithm maze =
    let startCell = maze.cells.[0]

    let mutable visited = Set.empty
    let mutable frontier : (Cell * Direction) list = []
    let mutable resultMaze = maze

    visited <- Set.add (startCell.X, startCell.Y) visited

    frontier <- getNeighbors resultMaze startCell
            |> List.map (fun (_,dir) -> (startCell, dir))

    while frontier.Length > 0 do
        let index = Random().Next(frontier.Length)
        let (cell, dir) = frontier.[index]
        frontier <- frontier |> List.mapi (fun i x -> (i,x)) |> List.filter (fun (i, _) -> i <> index) |> List.map snd

        let neighbors = getNeighbors resultMaze cell
        match neighbors |> List.tryFind (fun (_, d) -> d = dir) with
        | Some (neighbor, _) ->
            if not (Set.contains (neighbor.X, neighbor.Y) visited) then
                resultMaze <- removeWall resultMaze cell dir
                visited <- Set.add (neighbor.X, neighbor.Y) visited
                let newWalls = getNeighbors resultMaze neighbor |> List.filter (fun (n, _) -> not (Set.contains (n.X, n.Y) visited)) |> List.map (fun (_,d) -> (neighbor, d))
                frontier <- frontier @ newWalls
        | None -> ()
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

    let maze = primsAlgorithm maze

    let maze = removeOuterWall maze 0 0 Left

    let maze = removeOuterWall maze (maze.width - 1) (maze.height - 1) Left

    Console.SetWindowSize(800, 400);

    printMaze maze

    0