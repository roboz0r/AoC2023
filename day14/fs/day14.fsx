open System

let sample = """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""

let [<Literal>] RoundRock = 'O'
let [<Literal>] CubeRock = '#'
let [<Literal>] Empty = '.'

type Tilt =
    | North
    | East
    | South
    | West

type RockGrid = 
    {
        Grid: string[]
    }
    member this.Item with get (x, y) = this.Grid.[y].[x]
    member this.Width = this.Grid.[0].Length
    member this.Height = this.Grid.Length

module RockGrid = 
    let parse (input: String) = 
        let grid = 
            input.Split('\n')
            |> Array.filter (String.IsNullOrEmpty >> not)
        { Grid = grid }

    let getLoadNorth (grid: RockGrid) =
        let width = grid.Width
        let height = grid.Height
        let mutable load = 0
        for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                match grid.[x, y] with
                | RoundRock -> 
                    load <- load + (height - y)
                | _ -> ()
        load

    let as2DArray (grid: RockGrid) =
        let width = grid.Width
        let height = grid.Height
        let mutable newGrid = Array2D.zeroCreate width height
        for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                newGrid.[x, y] <- grid.[x, y]
        newGrid

    let of2DArray (grid: char[,]) =
        let width = grid.GetLength(0)
        let height = grid.GetLength(1)
        let newGrid = Array.init height (fun y -> 
            String(grid.[*, y])
        )
        { Grid = newGrid }

    let rec rollOneNorth x y (working: char[,]) =
        if y = 0 then ()
        else 
            match working.[x, y] with
            | RoundRock -> 
                match working[x, y - 1] with
                | Empty ->
                    working.[x, y] <- Empty
                    working.[x, y - 1] <- RoundRock
                    rollOneNorth x (y - 1) working
                | _ -> ()
            | _ -> failwith "Not a round rock"

    let rec rollOneEast x y (working: char[,]) =
        if x = working.GetLength(0) - 1 then ()
        else 
            match working.[x, y] with
            | RoundRock -> 
                match working[x + 1, y] with
                | Empty ->
                    working.[x, y] <- Empty
                    working.[x + 1, y] <- RoundRock
                    rollOneEast (x + 1) y working
                | _ -> ()
            | _ -> failwith "Not a round rock"

    let rec rollOneSouth x y (working: char[,]) =
        if y = working.GetLength(1) - 1 then ()
        else 
            match working.[x, y] with
            | RoundRock -> 
                match working[x, y + 1] with
                | Empty ->
                    working.[x, y] <- Empty
                    working.[x, y + 1] <- RoundRock
                    rollOneSouth x (y + 1) working
                | _ -> ()
            | _ -> failwith "Not a round rock"

    let rec rollOneWest x y (working: char[,]) =
        if x = 0 then ()
        else 
            match working.[x, y] with
            | RoundRock -> 
                match working[x - 1, y] with
                | Empty ->
                    working.[x, y] <- Empty
                    working.[x - 1, y] <- RoundRock
                    rollOneWest (x - 1) y working
                | _ -> ()
            | _ -> failwith "Not a round rock"
        

    let tiltNorthW width height (workingGrid: char[,]) =

        for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                match workingGrid.[x, y] with
                | RoundRock -> 
                    rollOneNorth x y workingGrid
                | _ -> ()
    let tiltEastW width height (workingGrid: char[,]) =
        for y in 0 .. height - 1 do
            for x in width - 1 .. -1 .. 0 do
                match workingGrid.[x, y] with
                | RoundRock -> 
                    rollOneEast x y workingGrid
                | _ -> ()

    let tiltSouthW width height (workingGrid: char[,]) =
        for x in 0 .. width - 1 do
            for y in height - 1 .. -1 .. 0 do
                match workingGrid.[x, y] with
                | RoundRock -> 
                    rollOneSouth x y workingGrid
                | _ -> ()

    let tiltWestW width height (workingGrid: char[,]) =
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                match workingGrid.[x, y] with
                | RoundRock -> 
                    rollOneWest x y workingGrid
                | _ -> ()

    let tiltNorth (grid: RockGrid) =
        let mutable workingGrid = as2DArray grid
        tiltNorthW grid.Width grid.Height workingGrid
        of2DArray workingGrid


    let tiltEast (grid: RockGrid) =
        let mutable workingGrid = as2DArray grid
        tiltEastW grid.Width grid.Height workingGrid
        of2DArray workingGrid

    let tiltSouth (grid: RockGrid) =
        let mutable workingGrid = as2DArray grid
        tiltSouthW grid.Width grid.Height workingGrid
        of2DArray workingGrid

    let tiltWest (grid: RockGrid) =
        let mutable workingGrid = as2DArray grid
        tiltWestW grid.Width grid.Height workingGrid
        of2DArray workingGrid

    let cycleW w h (workingGrid: char[,]) =
        tiltNorthW w h workingGrid
        tiltWestW w h workingGrid
        tiltSouthW w h workingGrid
        tiltEastW w h workingGrid

    let cycle (grid: RockGrid) =
        let mutable workingGrid = as2DArray grid
        let w, h = grid.Width, grid.Height
        cycleW w h workingGrid
        of2DArray workingGrid

    let cycleN n (grid: RockGrid) =
        let mutable workingGrid = as2DArray grid
        let w, h = grid.Width, grid.Height
        for i in 1 .. n do
            cycleW w h workingGrid
        of2DArray workingGrid

    let cycleUntilLoop (grid: RockGrid) =
        let w, h = grid.Width, grid.Height
        let mutable workingGrid = as2DArray grid
        let mutable workingGrid2 = as2DArray grid
        let mutable cycles = 0

        let loop () =
            cycleW w h workingGrid2
            cycleW w h workingGrid2
            cycleW w h workingGrid

            cycles <- cycles + 1
            while (workingGrid2 <> workingGrid) do
                cycleW w h workingGrid2
                cycleW w h workingGrid2
                cycleW w h workingGrid
                cycles <- cycles + 1
            // printfn "First Loop %i" cycles
            cycles
            
        let first = loop()
        let second = loop()

        of2DArray workingGrid, first, second

    let toString (grid: RockGrid) =
        String.Join("\n", grid.Grid)

let sampleTilted = """OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#...."""

let sampleP = sample |> RockGrid.parse
// sampleP 
// |> RockGrid.cycleUntilLoop
// |> (fun (grid, i) -> RockGrid.toString grid, i) |> (fun (s, i) -> printfn "%i\n%s" i s)
sampleTilted = (sampleP |> RockGrid.tiltNorth |> RockGrid.toString)

sampleP
|> RockGrid.tiltNorth
|> RockGrid.getLoadNorth

open System.IO


// inputP
// |> RockGrid.cycleUntilLoop
// |> (fun (grid, i) -> RockGrid.toString grid, i) |> (fun (s, i) -> printfn "%i\n%s" i s)

// cycles 126, 168, 210
let equivalentCycle total first second =
    // 126 + (1000000000 - 126) % (168 - 126) // = 160
    first + (total - first) % (second - first)

#time "on"
let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))
let parsed = input |> RockGrid.parse

parsed
|> RockGrid.tiltNorth
|> RockGrid.getLoadNorth

let loadNorth =
    parsed
    |> RockGrid.cycleUntilLoop
    |>(fun (grid, first, second) -> 
        let cycles = equivalentCycle 1_000_000_000 first second
        (RockGrid.cycleN cycles grid)
    )
    |> RockGrid.getLoadNorth
#time "off"
