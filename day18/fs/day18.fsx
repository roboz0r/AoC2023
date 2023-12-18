open System

let [<Literal>] Trench = '#'
let [<Literal>] Ground = '.'

type Point =
    {
        X: int
        Y: int
    }

type Direction =
    | Up
    | Down
    | Left
    | Right

type Instruction =
    {
        Direction: Direction
        Length: int
        Color: Drawing.Color
    }

module Instruction =
    // R 6 (#70c710)
    let parse (line: string) =
        let parts = line.Split(' ')
        let direction = 
            match parts.[0].[0] with
            | 'R' -> Right
            | 'L' -> Left
            | 'U' -> Up
            | 'D' -> Down
            | _ -> failwith "Invalid direction"
        let length = int parts.[1]
        let color = Drawing.ColorTranslator.FromHtml(parts.[2].Substring(1, 7))
        {
            Direction = direction
            Length = length
            Color = color
        }
    let parse2 (line: string) =
        let parts = line.Split(' ')
        let direction = 
            match parts.[2].[7] with
            | '0' -> Right
            | '1' -> Down
            | '2' -> Left
            | '3' -> Up
            | _ -> failwith "Invalid direction"
        let length = Convert.ToInt32(parts.[2].Substring(2, 5), 16)
        let color = Drawing.ColorTranslator.FromHtml(parts.[2].Substring(1, 7))
        {
            Direction = direction
            Length = length
            Color = color
        }

    let parseAll (input: string) =
        input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parse

    let parseAll2 (input: string) =
        input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parse2

type Trench =
    {
        StartX: int
        StartY: int
        Direction: Direction
        Length: int
        Color: Drawing.Color
    }

type State =
    {
        X: int
        Y: int
        Trenches: Trench list
    }
    static member Start =
        {
            X = 0
            Y = 0
            Trenches = []
        }

type TrenchGrid =
    {
        Grid: char [,]
    }
    member this.Item 
        with get(x, y) = this.Grid.[y, x]
        and set(x, y) v = this.Grid.[y, x] <- v
    member this.Width = this.Grid.GetLength(1)
    member this.Height = this.Grid.GetLength(0)
    member this.Copy() =
        {
            Grid = Array2D.copy this.Grid
        }

module State = 
    let addInstruction (x: Instruction) state =
        let trench =
            {
                StartX = state.X
                StartY = state.Y
                Direction = x.Direction
                Length = x.Length
                Color = x.Color
            }
        let x, y =
            match x.Direction with
            | Up -> state.X, state.Y - x.Length
            | Down -> state.X, state.Y + x.Length
            | Left -> state.X - x.Length, state.Y
            | Right -> state.X + x.Length, state.Y
        {
            X = x
            Y = y
            Trenches = trench :: state.Trenches
        }

    let minXY state = 
        let mutable x = state.X
        let mutable y = state.Y
        for trench in state.Trenches do
            y <- min y trench.StartY
            x <- min x trench.StartX
        x, y

    let maxXY state =
        let mutable x = state.X
        let mutable y = state.Y
        for trench in state.Trenches do
            y <- max y trench.StartY
            x <- max x trench.StartX
        x, y

    let offset (x, y) state =
        {
            X = state.X - x
            Y = state.Y - y
            Trenches = 
                state.Trenches 
                |> List.map (fun trench ->
                    {
                        trench with
                            StartX = trench.StartX - x
                            StartY = trench.StartY - y
                    })
        }

    let toTrenchGrid state = 
        let minX, minY = minXY state
        let maxX, maxY = maxXY state
        let width = maxX - minX + 1
        let height = maxY - minY + 1
        let offsetState = offset (minX, minY) state
        let grid = Array2D.init height width (fun _ _ -> Ground)
        let trenches = Array.ofList offsetState.Trenches
        for j in trenches.Length - 1 .. -1 .. 0 do
            let trench = trenches.[j]
            printfn $"trench:{trench}"
            let x, y = trench.StartX, trench.StartY
            let dx, dy =
                match trench.Direction with
                | Up -> 0, -1
                | Down -> 0, 1
                | Left -> -1, 0
                | Right -> 1, 0
            for i in 0 .. (trench.Length - 1) do
                printfn $"grid.[{y} + {i} * {dy}, {x} + {i} * {dx}] <- Trench"
                grid.[y + i * dy, x + i * dx] <- Trench

        { Grid = grid }

module TrenchGrid = 
    let render (grid: TrenchGrid) =
        for y in 0 .. grid.Height - 1 do
            for x in 0 .. grid.Width - 1 do
                printf "%c" grid.[x, y]
            printfn ""

    let [<Literal>] Outer = 'O'
    let [<Literal>] Inner = 'I'

    let private markOuterEdge (grid: TrenchGrid) =
        for x in 0 .. grid.Width - 1 do
            match grid.[x, 0] with
            | Ground -> grid.[x, 0] <- Outer
            | _ -> ()
            match grid.[x, grid.Height - 1] with
            | Ground -> grid.[x, grid.Height - 1] <- Outer
            | _ -> ()

        for y in 0 .. grid.Height - 1 do
            match grid.[0, y] with
            | Ground -> grid.[0, y] <- Outer
            | _ -> ()
            match grid.[grid.Width - 1, y] with
            | Ground -> grid.[grid.Width - 1, y] <- Outer
            | _ -> ()

    let getRegion (grid: TrenchGrid) start = 

        let isOuter x y = 
            match grid.[x, y] with
            | Outer -> true
            | _ -> false

        let isGroundOrOuter x y = 
            match grid.[x, y] with
            | Ground | Outer -> true
            | _ -> false

        let getNeighbours x y =
            [|
                if x > 0 then
                    { X = x - 1; Y = y }
                if x < grid.Width - 1 then
                    { X = x + 1; Y = y }
                if y > 0 then
                    { X = x; Y = y - 1 }
                if y < grid.Height - 1 then
                    { X = x; Y = y + 1 }
            |]

        let rec loop (visited: Set<Point>) toVisit =
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
                    |> Array.filter (fun { X = x; Y = y } -> isGroundOrOuter x y && not (Set.contains { X = x; Y = y } visited))
                    |> Array.fold (fun (visited, toVisit) n -> 
                        if isOuter n.X n.Y then
                            (Set.add n visited, toVisit)
                        else
                            (visited, Set.add n toVisit)
                        ) (visited, toVisit)
                loop visited toVisit

        loop Set.empty (Set.singleton start)

    let markInnerOuter (grid: TrenchGrid) =
        let grid = grid.Copy()
        markOuterEdge grid
        for y in 1 .. grid.Height - 2 do
            for x in 1 .. grid.Width - 2 do
                match grid.[x, y] with
                | Ground -> 
                    let region = getRegion grid { X = x; Y = y }
                    
                    let isOuter = region |> Seq.exists (fun { X = x; Y = y } -> grid.[x, y] = Outer)

                    for { X = x; Y = y } in region do
                        if isOuter then
                            grid.[x, y] <- Outer
                        else
                            grid.[x, y] <- Inner
                | _ -> ()
        grid

    let countExcavation (grid: TrenchGrid) =
        let mutable count = 0
        for y in 0 .. grid.Height - 1 do
            for x in 0 .. grid.Width - 1 do
                match grid.[x, y] with
                | Trench | Inner -> count <- count + 1
                | _ -> ()
        count

        

let sample = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"""

let parsedSample = Instruction.parseAll sample

let state = 
    parsedSample
    |> Array.fold (fun state x -> State.addInstruction x state) State.Start

let grid = State.toTrenchGrid state
grid.Width
grid.Height
TrenchGrid.render grid
let gridM = TrenchGrid.markInnerOuter grid
TrenchGrid.render gridM
TrenchGrid.countExcavation gridM
State.maxXY state
State.minXY state

// Part2

let parsedSample2 = Instruction.parseAll2 sample

let state2 = 
    parsedSample2
    |> Array.fold (fun state x -> State.addInstruction x state) State.Start

let shoelaceFormula (points: Point[]) =
    let mutable sum = 0L
    for i in 0 .. points.Length - 1 do
        let j = (i + 1) % points.Length
        let p1 = points.[i]
        let p2 = points.[j]
        sum <- sum + (int64 p1.X * int64 p2.Y - int64 p2.X * int64 p1.Y)
    abs sum / 2L

let totalArea (state: State) =
    let points =
        let ts = Array.ofList state.Trenches
        Array.init (ts.Length + 1) (fun i -> 
            if i = 0 then {X = state.X; Y= state.Y}
            else {X = ts.[i - 1].StartX; Y = ts.[i - 1].StartY})
    let innerArea = shoelaceFormula points
    
    let perimeter =
        state.Trenches
        |> List.sumBy (fun t -> int64 t.Length)

    // Not sure why the +1 is needed, but it is
    innerArea + perimeter / 2L + 1L

totalArea state2

open System.IO
// Part1

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))
let parsedInput = Instruction.parseAll input
let state1 = 
    parsedInput
    |> Array.fold (fun state x -> State.addInstruction x state) State.Start
let grid1 = State.toTrenchGrid state1
grid1.Width
grid1.Height
TrenchGrid.render grid1
let grid1M = TrenchGrid.markInnerOuter grid1
TrenchGrid.countExcavation grid1M



// Part2

let parsedInput2 = Instruction.parseAll2 input

parsedInput2 |> Array.map (fun x -> x.Length) |> Array.sort
let stateInput2 = 
    parsedInput2
    |> Array.fold (fun state x -> State.addInstruction x state) State.Start

totalArea stateInput2
