open System

// Types
type Direction = Up | Down | Left | Right

type Cell = { X: int; Y: int; Walls: Set<Direction> }

type Wall = Cell * Direction

type Maze = { width: int; height: int; cells: Cell[,] }

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
        Array2D.init width height (fun x y ->
            { X = x
              Y = y
              Walls = Set.ofList allWalls })
    { width = width; height = height; cells = cells }

// Get cell from maze
let getCell maze x y =
    maze.cells[x, y]

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
    let (dx, dy) =
        match direction with
        | Up -> (0, -1)
        | Down -> (0, 1)
        | Left -> (-1, 0)
        | Right -> (1, 0)

    let nx = cell.X + dx
    let ny = cell.Y + dy

    if nx >= 0 && nx < maze.width && ny >= 0 && ny < maze.height then
        let oppositeDir = oppositeDirection direction

        let current = maze.cells[cell.X, cell.Y]
        maze.cells[cell.X, cell.Y] <-
            { current with Walls = Set.remove direction current.Walls }

        let neighbor = maze.cells[nx, ny]
        maze.cells[nx, ny] <-
            { neighbor with Walls = Set.remove oppositeDir neighbor.Walls }

    maze

// Remove outer wall of a cell (used for entrance and exit)
let removeOuterWall maze x y direction =
    let cell = maze.cells[x, y]
    maze.cells[x, y] <-
        { cell with Walls = Set.remove direction cell.Walls }
    maze

// Random generator (single instance!)
let random = Random()

let trueRandom maze =
    let cellsSeq = seq { for x in 0 .. maze.width - 1 do for y in 0 .. maze.height - 1 -> maze.cells[x, y] }
    Seq.fold (fun accMaze cell ->
        let neighbors = getNeighbors accMaze cell
        if neighbors.Length > 0 then
            let (_, dir) = neighbors.[random.Next(neighbors.Length)]
            removeWall accMaze cell dir
        else
            accMaze
    ) maze cellsSeq


// Prim's algorithm
let primsAlgorithm maze =
    let startCell = maze.cells[0, 0]

    let mutable visited = Set.empty
    let mutable frontier : (Cell * Direction) list = []

    visited <- Set.add (startCell.X, startCell.Y) visited

    frontier <-
        getNeighbors maze startCell
        |> List.map (fun (_, dir) -> (startCell, dir))

    while frontier.Length > 0 do
        let index = random.Next(frontier.Length)
        let (cell, dir) = frontier[index]

        // Remove selected wall from frontier
        frontier <-
            frontier
            |> List.mapi (fun i x -> (i, x))
            |> List.filter (fun (i, _) -> i <> index)
            |> List.map snd

        match getNeighbors maze cell |> List.tryFind (fun (_, d) -> d = dir) with
        | Some (neighbor, _) ->
            if not (Set.contains (neighbor.X, neighbor.Y) visited) then

                removeWall maze cell dir |> ignore

                visited <- Set.add (neighbor.X, neighbor.Y) visited

                let newWalls =
                    getNeighbors maze neighbor
                    |> List.filter (fun (n, _) ->
                        not (Set.contains (n.X, n.Y) visited))
                    |> List.map (fun (_, d) -> (neighbor, d))

                frontier <- frontier @ newWalls
        | None -> ()

    maze

// Lazy version of Prim's algorithm that yields the maze state after every change so that step by step visualisation is possible
let primsAlgorithmLazy maze =
    seq {

        let startCell = maze.cells[0, 0]

        let mutable visited = Set.empty
        let mutable frontier : (Cell * Direction) list = []

        visited <- Set.add (startCell.X, startCell.Y) visited

        frontier <-
            getNeighbors maze startCell
            |> List.map (fun (_, dir) -> (startCell, dir))

        // Yield initial maze state
        yield maze

        while frontier.Length > 0 do
            let index = random.Next(frontier.Length)
            let (cell, dir) = frontier[index]

            frontier <-
                frontier
                |> List.mapi (fun i x -> (i, x))
                |> List.filter (fun (i, _) -> i <> index)
                |> List.map snd

            match getNeighbors maze cell |> List.tryFind (fun (_, d) -> d = dir) with
            | Some (neighbor, _) ->
                if not (Set.contains (neighbor.X, neighbor.Y) visited) then

                    removeWall maze cell dir |> ignore

                    visited <- Set.add (neighbor.X, neighbor.Y) visited

                    let newWalls =
                        getNeighbors maze neighbor
                        |> List.filter (fun (n, _) ->
                            not (Set.contains (n.X, n.Y) visited))
                        |> List.map (fun (_, d) -> (neighbor, d))

                    frontier <- frontier @ newWalls

                    // yield after every change
                    yield maze
            | None -> ()
    }

// Eller's algorithm
let ellersAlgorithm maze =
    let width = maze.width
    let height = maze.height

    let random = Random()

    // Each row has a set ID per column
    let mutable nextSetId = 1
    let mutable sets = Array.zeroCreate width

    // Helper to merge two sets
    let mergeSets a b =
        let oldSet = sets[b]
        let newSet = sets[a]
        for i in 0 .. width - 1 do
            if sets[i] = oldSet then
                sets[i] <- newSet

    for y in 0 .. height - 1 do

        // Assign sets if not assigned
        for x in 0 .. width - 1 do
            if sets[x] = 0 then
                sets[x] <- nextSetId
                nextSetId <- nextSetId + 1

        // Join adjacent cells randomly (except last row)
        if y <> height - 1 then
            for x in 0 .. width - 2 do
                if sets[x] <> sets[x + 1] && random.Next(2) = 0 then
                    removeWall maze maze.cells[x, y] Right |> ignore
                    mergeSets x (x + 1)

        else
            // Force merge all different sets on last row
            for x in 0 .. width - 2 do
                if sets[x] <> sets[x + 1] then
                    removeWall maze maze.cells[x, y] Right |> ignore
                    mergeSets x (x + 1)

        // Create vertical connections (except last row)
        if y <> height - 1 then

            let newSets = Array.zeroCreate width

            // Group columns by set
            let grouped =
                sets
                |> Array.mapi (fun i s -> (i, s))
                |> Array.groupBy snd

            for (_, members) in grouped do

                // At least one cell per set must connect downward
                let connectCount =
                    1 + random.Next(members.Length)

                let shuffled =
                    members |> Array.sortBy (fun _ -> random.Next())

                for i in 0 .. connectCount - 1 do
                    let (x, setId) = shuffled[i]
                    removeWall maze maze.cells[x, y] Down |> ignore
                    newSets[x] <- setId

            sets <- newSets

    maze


let ellersAlgorithmLazy maze =
    seq {
        let width = maze.width
        let height = maze.height

        let random = Random()

        // Each row has a set ID per column
        let mutable nextSetId = 1
        let mutable sets = Array.zeroCreate width

        // Yield initial maze state
        yield maze

        // Helper to merge two sets
        let mergeSets a b =
            let oldSet = sets[b]
            let newSet = sets[a]
            for i in 0 .. width - 1 do
                if sets[i] = oldSet then
                    sets[i] <- newSet

        for y in 0 .. height - 1 do

            // Assign sets if not assigned
            for x in 0 .. width - 1 do
                if sets[x] = 0 then
                    sets[x] <- nextSetId
                    nextSetId <- nextSetId + 1

            // Join adjacent cells randomly (except last row)
            if y <> height - 1 then
                for x in 0 .. width - 2 do
                    if sets[x] <> sets[x + 1] && random.Next(2) = 0 then
                        removeWall maze maze.cells[x, y] Right |> ignore
                        mergeSets x (x + 1)
                        yield maze

            else
                // Force merge all different sets on last row
                for x in 0 .. width - 2 do
                    if sets[x] <> sets[x + 1] then
                        removeWall maze maze.cells[x, y] Right |> ignore
                        mergeSets x (x + 1)
                        yield maze

            // Create vertical connections (except last row)
            if y <> height - 1 then

                let newSets = Array.zeroCreate width

                // Group columns by set
                let grouped =
                    sets
                    |> Array.mapi (fun i s -> (i, s))
                    |> Array.groupBy snd

                for (_, members) in grouped do

                    // At least one cell per set must connect downward
                    let connectCount =
                        1 + random.Next(members.Length)

                    let shuffled =
                        members |> Array.sortBy (fun _ -> random.Next())

                    for i in 0 .. connectCount - 1 do
                        let (x, setId) = shuffled[i]
                        removeWall maze maze.cells[x, y] Down |> ignore
                        newSets[x] <- setId
                        yield maze

                sets <- newSets

        yield maze
    }


// ASCII Visualisation
let printMaze maze =
    let horizontalWall = "+---"
    let noHorizontalWall = "+   "
    let verticalWall = "|   "
    let noVerticalWall = "    "

    for y in 0 .. maze.height - 1 do

        // Top walls
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells[x, y]
            if Set.contains Up cell.Walls then
                printf "%s" horizontalWall
            else
                printf "%s" noHorizontalWall
        printfn "+"

        // Left walls
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells[x, y]
            if Set.contains Left cell.Walls then
                printf "%s" verticalWall
            else
                printf "%s" noVerticalWall

        // Right wall of last cell
        let lastCell = maze.cells[maze.width - 1, y]
        if Set.contains Right lastCell.Walls then
            printfn "|"
        else
            printfn " "

    // Bottom border
    for x in 0 .. maze.width - 1 do
        let cell = maze.cells[x, maze.height - 1]
        if Set.contains Down cell.Walls then
            printf "%s" horizontalWall
        else
            printf "%s" noHorizontalWall
    printfn "+"

// Main
[<EntryPoint>]
let main argv =

    let maze = createMaze 10 10

    //let maze = trueRandom maze

    //let maze = primsAlgorithm maze

    //for step in primsAlgorithmLazy maze do
    //    Console.Clear()
    //    printMaze step
    //    System.Threading.Thread.Sleep(1000)


    //let maze = ellersAlgorithm maze

    let maze = removeOuterWall maze 0 0 Left

    let maze = removeOuterWall maze (maze.width - 1) (maze.height - 1) Right

    for step in ellersAlgorithmLazy maze do
        Console.Clear()
        printMaze step
        System.Threading.Thread.Sleep(200)

    //Console.SetWindowSize(800, 400);

    printMaze maze

    0