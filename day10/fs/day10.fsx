open System

type Pipe =
    | NorthSouth
    | EastWest
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    | Ground
    | Start


type Cardinal = 
    | North
    | East
    | South
    | West

type Position = 
    {X:int;  Y:int}
    static member Of(x, y) = { X = x; Y = y }

type PipeGrid = 
    {
        Grid: Pipe [] []
    }
    member this.Item with get (x, y) = this.Grid.[y].[x]
    member this.XLength = this.Grid.[0].Length
    member this.YLength = this.Grid.Length

type DistanceGrid = 
    {
        Grid: int [] []
    }
    member this.Item 
        with get (x, y) = this.Grid.[y].[x]
        and set (x, y) v = this.Grid.[y].[x] <- v
    member this.XLength = this.Grid.[0].Length
    member this.YLength = this.Grid.Length
    member this.IsUnvisited { X = x; Y = y } = this.Grid.[y].[x] = -1
    member this.Visit { X = x; Y = y } dist = this.Grid.[y].[x] <- dist
    static member Create (grid: PipeGrid) { X = x; Y = y } =
        {
            Grid = Array.init grid.YLength (fun iY -> 
                Array.init grid.XLength (fun iX -> 
                    if iX = x && iY = y then 0
                    else -1
            ))
        }

type DistanceFromOrigin = 
    {
        Position: Position
        Distance: int
        From: Cardinal
    }

module Pipe =
    // | is a vertical pipe connecting north and south.
    // - is a horizontal pipe connecting east and west.
    // L is a 90-degree bend connecting north and east.
    // J is a 90-degree bend connecting north and west.
    // 7 is a 90-degree bend connecting south and west.
    // F is a 90-degree bend connecting south and east.
    // . is ground; there is no pipe in this tile.
    // S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
    let parse (c: char) =
        match c with
        | '|' -> NorthSouth
        | '-' -> EastWest
        | 'L' -> NorthEast
        | 'J' -> NorthWest
        | '7' -> SouthWest
        | 'F' -> SouthEast
        | '.' -> Ground
        | 'S' -> Start
        | _ -> failwith "Invalid pipe"

    let toChar (p: Pipe) =
        match p with
        | NorthSouth -> '|'
        | EastWest -> '-'
        | NorthEast -> 'L'
        | NorthWest -> 'J'
        | SouthWest -> '7'
        | SouthEast -> 'F'
        | Ground -> '.'
        | Start -> 'S'

module PipeGrid =
    let tryValidateNextConnection (grid: PipeGrid) (distGrid: DistanceGrid) from ({X = x;  Y = y } as pos) dist =
        if distGrid.IsUnvisited pos then
            // printfn "Validating %A" pos
            match from, grid[x,y] with
            | North, NorthSouth
            | North, NorthEast
            | North, NorthWest
            | East, EastWest
            | East, NorthEast
            | East, SouthEast
            | South, NorthSouth
            | South, SouthEast
            | South, SouthWest
            | West, EastWest
            | West, NorthWest
            | West, SouthWest ->
                distGrid.Visit pos dist
                Some { 
                    Distance = dist
                    Position = { X = x; Y = y }
                    From = from
                }
            | _ -> None
        else None

    let findConnectionsToStart (grid: PipeGrid) distGrid ({X = startX;  Y = startY }) =
        [|
            (startX - 1, startY)
            (startX + 1, startY)
            (startX, startY - 1)
            (startX, startY + 1)
        |] |> Array.choose (fun (x, y) ->
            if x >= 0 && x < grid.XLength && y >= 0 && y < grid.YLength then
                let from =
                    if x < startX then East
                    elif x > startX then West
                    elif y < startY then South
                    else North

                tryValidateNextConnection grid distGrid from { X = x; Y = y } 1
            else
                None)

    let findNextConnection (grid: PipeGrid) (distGrid: DistanceGrid) (connections: DistanceFromOrigin list) =
        match connections with
        | [] -> failwith "NO CONNECTIONS"
        | connection :: _ ->
            let pos = connection.Position
            let nextDist = connection.Distance + 1
            
            match connection.From, grid[pos.X, pos.Y] with
            | North, NorthSouth -> tryValidateNextConnection grid distGrid North { pos with Y = pos.Y + 1 } nextDist
            | North, NorthEast -> tryValidateNextConnection grid distGrid West { pos with X = pos.X + 1 } nextDist
            | North, NorthWest -> tryValidateNextConnection grid distGrid East { pos with X = pos.X - 1 } nextDist
            | East, EastWest -> tryValidateNextConnection grid distGrid East { pos with X = pos.X - 1 } nextDist
            | East, NorthEast -> tryValidateNextConnection grid distGrid South { pos with Y = pos.Y - 1 } nextDist
            | East, SouthEast -> tryValidateNextConnection grid distGrid North { pos with Y = pos.Y + 1 } nextDist
            | South, NorthSouth -> tryValidateNextConnection grid distGrid South { pos with Y = pos.Y - 1 } nextDist
            | South, SouthEast -> tryValidateNextConnection grid distGrid West { pos with X = pos.X + 1 } nextDist
            | South, SouthWest -> tryValidateNextConnection grid distGrid East { pos with X = pos.X - 1 } nextDist
            | West, EastWest -> tryValidateNextConnection grid distGrid West { pos with X = pos.X + 1 } nextDist
            | West, NorthWest -> tryValidateNextConnection grid distGrid South { pos with Y = pos.Y - 1 } nextDist
            | West, SouthWest -> tryValidateNextConnection grid distGrid North { pos with Y = pos.Y + 1 } nextDist
            | _ -> failwith "Invalid connection"




let sample =
    """7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"""

let parseGrid (input: string) =
    let mutable start = None
    let grid = 
        input.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.mapi (fun y line ->
            line.ToCharArray()
            |> Array.mapi (fun x c ->
                match Pipe.parse c with
                | Start ->
                    start <- Some { X = x; Y = y}
                    Start
                | p -> p))
    Option.get start, {
        PipeGrid.Grid = grid
    }, DistanceGrid.Create { PipeGrid.Grid = grid } (Option.get start)

let printMaxDistance (input:string) =

    let (start, pipeGrid, distGrid) = parseGrid input

    let mutable runs =
        PipeGrid.findConnectionsToStart pipeGrid distGrid start
        |> Array.map (fun x -> true, List.singleton x)

    let anyTrue (xs: (bool * 'a list) []) =
        xs |> Array.exists fst

    while anyTrue runs do
        runs <- 
            runs
            |> Array.map (fun (b, xs) ->
                if b then
                    let next = PipeGrid.findNextConnection pipeGrid distGrid xs
                    match next with
                    | Some x -> (true, x :: xs)
                    | None -> (false, xs)
                else (b, xs))

    printfn "Max distance %d" (runs |> Array.map (snd >> List.head >> _.Distance) |> Array.max)
    distGrid, runs

open System.IO

module DistanceGrid =
    let [<Literal>] Inner = -73
    let [<Literal>] Outer = -79
    let [<Literal>] Ground = -10
    let [<Literal>] Undefined = -20
    let [<Literal>] Region = -30

    let expandGrid (grid: DistanceGrid) =
        let newGrid = 
            Array.init (grid.YLength * 2 - 1) (fun y ->
                Array.init (grid.XLength * 2 - 1) (fun x ->
                    if (x % 2 = 0) && (y % 2 = 0) then
                        match grid.[x / 2, y / 2] with
                        | -1 -> Ground
                        | x -> x * 2
                    else 
                        Undefined
                ))    
            |> fun x -> {DistanceGrid.Grid = x}

        for y in 0 .. (newGrid.YLength - 1) do
            for x in 0 .. (newGrid.XLength - 1) do
                let v = newGrid.[x, y]
                match v with
                | Undefined ->
                    if x = 0 || x = newGrid.XLength - 1 || y = 0 || y = newGrid.YLength - 1 then
                        newGrid.[x,y] <- Outer

                    if x % 2 = 1 then 
                        let w = newGrid.[x - 1, y]
                        let e = newGrid.[x + 1, y]
                        let ew = abs (e - w)
                        if ew = 2 then
                            newGrid.[x,y] <- (min e w) + 1
                        
                    elif y % 2 = 1 then
                        let n = newGrid.[x, y - 1]
                        let s = newGrid.[x, y + 1]
                        let ns = abs (n - s)
                        if ns = 2 then
                            newGrid.[x,y] <- (min n s) + 1

                    else ()
                | _ -> ()
        newGrid

    let getRegion (grid: DistanceGrid) start = 

        let isOuter x y = 
            match grid.[x, y] with
            | Outer -> true
            | _ -> false

        let isGroundOrUndefinedOrOuter x y = 
            match grid.[x, y] with
            | Ground | Undefined | Outer -> true
            | _ -> false

        let getNeighbours x y =
            [|
                if x > 0 then
                    { X = x - 1; Y = y }
                if x < grid.XLength - 1 then
                    { X = x + 1; Y = y }
                if y > 0 then
                    { X = x; Y = y - 1 }
                if y < grid.YLength - 1 then
                    { X = x; Y = y + 1 }
            |]

        let rec loop visited toVisit =
            toVisit
            |> Seq.tryHead
            |> function
            | None -> visited
            | Some pos ->
                let visited = Set.add pos visited
                let toVisit = Set.remove pos toVisit
                let neighbours = getNeighbours pos.X pos.Y
                let (visited, toVisit) = 
                    neighbours
                    |> Array.filter (fun { X = x; Y = y } -> isGroundOrUndefinedOrOuter x y && not (Set.contains { X = x; Y = y } visited))
                    |> Array.fold (fun (visited, toVisit) n -> 
                        if isOuter n.X n.Y then
                            (Set.add n visited, toVisit)
                        else
                            (visited, Set.add n toVisit)
                        ) (visited, toVisit)
                loop visited toVisit

        loop Set.empty (Set.singleton start)
            



    let markInnerOuter (grid: DistanceGrid) =
        for y in 0 .. (grid.YLength - 1) do
            for x in 0 .. (grid.XLength - 1) do
                match grid.[x, y] with
                | Ground -> 
                    let region = getRegion grid (Position.Of(x, y))
                    
                    let isOuter = region |> Seq.exists (fun { X = x; Y = y } -> grid.[x, y] = Outer)

                    for { X = x; Y = y } in region do
                        if isOuter then
                            grid.[x, y] <- Outer
                        else
                            grid.[x, y] <- Inner
                | _ -> ()

        grid

    let countInner (grid: DistanceGrid) =
        let mutable count = 0
        for y in 0 .. 2 .. (grid.YLength - 1) do
            for x in 0 .. 2 .. (grid.XLength - 1) do
                if grid.[x, y] = Inner then
                    count <- count + 1

        count

    let countGround (grid: DistanceGrid) =
        let mutable count = 0
        for y in 0 .. (grid.YLength - 1) do
            for x in 0 .. (grid.XLength - 1) do
                if grid.[x, y] = Ground then
                    count <- count + 1

        count

    let countMin1 (grid: DistanceGrid) =
        let mutable count = 0
        for y in 0 .. (grid.YLength - 1) do
            for x in 0 .. (grid.XLength - 1) do
                if grid.[x, y] = -1 then
                    count <- count + 1

        count

    let writeGrid (pGrid:PipeGrid) (grid: DistanceGrid) =
        use writer = new StreamWriter(Path.Combine(__SOURCE_DIRECTORY__, "grid.txt"))
        for y in 0 .. (grid.YLength - 1) do
            for x in 0 .. (grid.XLength - 1) do
                if (x % 2 = 0) && (y % 2 = 0) then
                    writer.Write(Pipe.toChar pGrid.[x / 2, y / 2])
                else
                    match grid.[x, y] with
                    | Outer -> writer.Write('O')
                    | _ -> writer.Write('x')
            writer.Write('\n')

    let writeGridNos (grid: DistanceGrid) =
        use writer = new StreamWriter(Path.Combine(__SOURCE_DIRECTORY__, "grid.txt"))
        for y in 0 .. (grid.YLength - 1) do
            for x in 0 .. (grid.XLength - 1) do
                writer.Write(sprintf "%0+6i" (grid.[x, y]))
            writer.Write('\n')

#time "on"
let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))
let distGrid, runs = printMaxDistance input
let (start, pipeGrid, distGridx) = parseGrid input
distGrid |> DistanceGrid.writeGridNos
distGrid 
|> DistanceGrid.expandGrid 
|> (fun x -> x.Grid |> Array.concat |> Set.ofArray |> Array.ofSeq) 
|> (fun x -> 
    x 
    |> Array.iteri (fun i v -> 
        if i = 0 then 
            () 
        elif v - (x[i - 1]) <> 1 then 
            printfn "%d %d" x[i - 1] x[i] 
        else ()
        
))
#time "off"

let distGridCount = distGrid |> DistanceGrid.expandGrid |> DistanceGrid.markInnerOuter |> DistanceGrid.countInner |> (fun x -> printfn "Inner: %d" x; x)

let sample2 = """...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."""

let (distGrid2, _) = printMaxDistance sample2
let (start2, pipeGrid2, _) = parseGrid sample2

distGrid2 
|> DistanceGrid.expandGrid
|> DistanceGrid.markInnerOuter
|> DistanceGrid.countInner

let sample3 = """..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
.........."""

let (distGrid3, _) = printMaxDistance sample3
distGrid3 
|> DistanceGrid.expandGrid
|> DistanceGrid.markInnerOuter
|> DistanceGrid.countInner

let sample4 = """.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."""

let (distGrid4, _) = printMaxDistance sample4
let (start4, pipeGrid4, _) = parseGrid sample4
distGrid4 
|> DistanceGrid.expandGrid
|> DistanceGrid.markInnerOuter
|> DistanceGrid.countInner

let (distGridPart2, _) = printMaxDistance input
distGridPart2 
|> DistanceGrid.expandGrid
|> DistanceGrid.markInnerOuter
|> DistanceGrid.countInner
