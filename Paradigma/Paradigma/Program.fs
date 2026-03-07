open System
// -----------------------------
// Type definitions
// -----------------------------

// Direction represents the four possible directions in the maze.
// These are used to determine neighbors and walls between cells.
type Direction = Up | Down | Left | Right

// A Cell represents one position in the maze grid.
// X and Y store the coordinates of the cell.
// Walls is a set containing the directions where walls still exist.
type Cell = { X: int; Y: int; Walls: Set<Direction> }

// A Wall is represented as a tuple containing a cell and the direction of the wall.
type Wall = Cell * Direction

// Maze represents the entire grid of cells.
type Maze = { width: int; height: int; cells: Cell[,] }


// -----------------------------
// Helper values and functions
// -----------------------------

// List containing all possible wall directions.
// Used when creating a new cell so that each cell initially has all walls.
let allWalls = [Up; Down; Left; Right]

// Returns the opposite direction of a given direction.
// Pattern matching is used because it clearly expresses the mapping.
let oppositeDirection dir =
    match dir with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left


// -----------------------------
// Maze creation
// -----------------------------

// Creates a new maze where every cell initially has all four walls.
let createMaze width height =
    let cells =
        // Array2D.init constructs the 2D grid by applying a function to each coordinate.
        Array2D.init width height (fun x y ->
            { X = x
              Y = y
              // Each cell starts with all walls intact.
              Walls = Set.ofList allWalls })
    { width = width; height = height; cells = cells }


// Returns a cell from the maze at the given coordinates.
let getCell maze x y =
    maze.cells[x, y]


// -----------------------------
// Neighbor lookup
// -----------------------------

// Returns all valid neighboring cells of a given cell.
let getNeighbors maze cell =
    let directions = [Up; Down; Left; Right]

    directions
    |> List.map (fun dir ->
        // Convert direction to coordinate change
        let (dx, dy) =
            match dir with
            | Up -> (0, -1)
            | Down -> (0, 1)
            | Left -> (-1, 0)
            | Right -> (1, 0)

        let neighborX = cell.X + dx
        let neighborY = cell.Y + dy

        // Only return neighbors that stay inside the maze boundaries
        if neighborX >= 0 && neighborX < maze.width && neighborY >= 0 && neighborY < maze.height then
            Some (getCell maze neighborX neighborY, dir)
        else
            None)
    // List.choose removes None values and unwraps Some values.
    |> List.choose id


// -----------------------------
// Wall removal
// -----------------------------

// Removes the wall between a cell and its neighbor.
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

        // Remove wall from current cell
        let current = maze.cells[cell.X, cell.Y]
        maze.cells[cell.X, cell.Y] <-
            { current with Walls = Set.remove direction current.Walls }

        // Remove opposite wall from neighboring cell
        let neighbor = maze.cells[nx, ny]
        maze.cells[nx, ny] <-
            { neighbor with Walls = Set.remove oppositeDir neighbor.Walls }

    maze


// Removes a wall on the outer boundary of the maze (used for entrance/exit)
let removeOuterWall maze x y direction =
    let cell = maze.cells[x, y]
    maze.cells[x, y] <-
        { cell with Walls = Set.remove direction cell.Walls }
    maze

// Single random generator instance
let random = Random()

// -----------------------------
// True random algorithm
// -----------------------------

// True random maze generation by randomly removing walls between cells.
let trueRandom maze =
    // Create a sequence of all cells in the maze.
    let cellsSeq = seq { for x in 0 .. maze.width - 1 do for y in 0 .. maze.height - 1 -> maze.cells[x, y] }
    // Fold over the sequence of cells, randomly removing walls where possible.
    Seq.fold (fun accMaze cell ->
        let neighbors = getNeighbors accMaze cell
        if neighbors.Length > 0 then
            let (_, dir) = neighbors.[random.Next(neighbors.Length)]
            removeWall accMaze cell dir
        else
            accMaze
    ) maze cellsSeq


// -----------------------------
// Prim's algorithm (lazy version)
// -----------------------------

// Lazy version of Prim's algorithm.
// Instead of returning the final maze immediately, this function yields
// intermediate states whenever a wall is removed.
let primsAlgorithmLazy maze =
    seq {

        let startCell = maze.cells[0, 0]

        // Set containing visited cells
        let mutable visited = Set.empty

        // Frontier contains walls that connect visited cells to unvisited ones
        let mutable frontier : (Cell * Direction) list = []

        visited <- Set.add (startCell.X, startCell.Y) visited

        // Initialize frontier with walls of the start cell
        frontier <-
            getNeighbors maze startCell
            |> List.map (fun (_, dir) -> (startCell, dir))

        // Yield the initial maze state
        yield maze

        while frontier.Length > 0 do
            let index = random.Next(frontier.Length)
            let (cell, dir) = frontier[index]

            // Remove selected wall from frontier list
            frontier <-
                frontier
                |> List.mapi (fun i x -> (i, x))
                |> List.filter (fun (i, _) -> i <> index)
                |> List.map snd

            match getNeighbors maze cell |> List.tryFind (fun (_, d) -> d = dir) with
            | Some (neighbor, _) ->

                // If neighbor was not visited yet, carve passage
                if not (Set.contains (neighbor.X, neighbor.Y) visited) then

                    removeWall maze cell dir |> ignore

                    visited <- Set.add (neighbor.X, neighbor.Y) visited

                    // Add new frontier walls
                    let newWalls =
                        getNeighbors maze neighbor
                        |> List.filter (fun (n, _) ->
                            not (Set.contains (n.X, n.Y) visited))
                        |> List.map (fun (_, d) -> (neighbor, d))

                    frontier <- frontier @ newWalls

                    // Yield maze after each modification
                    yield maze
            | None -> ()
    }


// -----------------------------
// Eller's algorithm (lazy)
// -----------------------------

// Lazy implementation of Eller's algorithm.
// The maze is generated row by row.
let ellersAlgorithmLazy maze =
    seq {

        let width = maze.width
        let height = maze.height

        let random = Random()

        // Array storing set ID for each column in the current row
        let mutable nextSetId = 1
        let mutable sets = Array.zeroCreate width

        // Yield the initial maze
        yield maze

        // Merges two sets when cells become connected
        let mergeSets a b =
            let oldSet = sets[b]
            let newSet = sets[a]

            // Replace all occurrences of the old set ID
            for i in 0 .. width - 1 do
                if sets[i] = oldSet then
                    sets[i] <- newSet

        for y in 0 .. height - 1 do

            // Assign new set IDs where needed
            for x in 0 .. width - 1 do
                if sets[x] = 0 then
                    sets[x] <- nextSetId
                    nextSetId <- nextSetId + 1

            // Horizontal connections
            if y <> height - 1 then
                for x in 0 .. width - 2 do
                    if sets[x] <> sets[x + 1] && random.Next(2) = 0 then
                        removeWall maze maze.cells[x, y] Right |> ignore
                        mergeSets x (x + 1)
                        yield maze

            // Final row must merge all remaining sets
            else
                for x in 0 .. width - 2 do
                    if sets[x] <> sets[x + 1] then
                        removeWall maze maze.cells[x, y] Right |> ignore
                        mergeSets x (x + 1)
                        yield maze

            // Vertical connections
            if y <> height - 1 then
                
                let newSets = Array.zeroCreate width

                // Group columns by their set ID to ensure at least one vertical connection per set
                let grouped =
                    sets
                    |> Array.mapi (fun i s -> (i, s))
                    |> Array.groupBy snd

                // For each set, randomly connect some cells downwards to the next row
                for (_, members) in grouped do

                    let connectCount =
                        1 + random.Next(members.Length)

                    // Shuffle members to randomize which cells get connected downwards
                    let shuffled =
                        members |> Array.sortBy (fun _ -> random.Next())

                    for i in 0 .. connectCount - 1 do
                        let (x, setId) = shuffled[i]
                        removeWall maze maze.cells[x, y] Down |> ignore
                        newSets[x] <- setId
                        yield maze

                sets <- newSets
    }


// -----------------------------
// ASCII visualisation
// -----------------------------

// Prints the maze using ASCII characters.
let printMaze maze =
    let horizontalWall = "+---"
    let noHorizontalWall = "+   "
    let verticalWall = "|   "
    let noVerticalWall = "    "

    for y in 0 .. maze.height - 1 do

        // Print top walls
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells[x, y]
            if Set.contains Up cell.Walls then
                printf "%s" horizontalWall
            else
                printf "%s" noHorizontalWall
        printfn "+"

        // Print left walls
        for x in 0 .. maze.width - 1 do
            let cell = maze.cells[x, y]
            if Set.contains Left cell.Walls then
                printf "%s" verticalWall
            else
                printf "%s" noVerticalWall

        let lastCell = maze.cells[maze.width - 1, y]

        if Set.contains Right lastCell.Walls then
            printfn "|"
        else
            printfn " "

    // Print bottom border
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

    //printMaze maze

    0