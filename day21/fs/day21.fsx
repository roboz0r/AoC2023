open System

//his starting position (S), garden plots (.), and rocks (#)

let [<Literal>] Start = 'S'
let [<Literal>] GardenPlot = '.'
let [<Literal>] Rock = '#'
let [<Literal>] PossibleLocation = 'O'

let sample = """...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"""

type GardenMap = 
    {
        Grid: string[]
    }
    member this.Width = this.Grid.[0].Length
    member this.Height = this.Grid.Length
    member this.Item with get (x, y) = this.Grid.[y].[x]

[<Struct>]
type Point =
    {
        X: int
        Y: int
    }

type State = 
    {
        Step: int
        Positions: Set<Point>
        Map: GardenMap
    }

module GardenMap =
    let possibleNextPosition { X = x; Y = y } (map: GardenMap) = 
        [|
            (x, y - 1)
            (x, y + 1)
            (x - 1, y)
            (x + 1, y)
        |] |> Array.choose (fun (x, y) -> 
            if x >= 0 && x < map.Width && y >= 0 && y < map.Height then 
                match map.[x, y] with
                | GardenPlot | Start -> Some { X = x; Y = y }
                | _ -> None
            else None
        )

    let getStart (map: GardenMap) =
        let rec loop x y =
            if x >= map.Width then
                loop 0 (y + 1)
            elif y >= map.Height then
                failwith "No start found"
            else
                match map.[x, y] with
                | Start -> { X = x; Y = y }
                | _ -> loop (x + 1) y
        loop 0 0

    let parse (input:string) =
        let grid = input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        { Grid = grid }

module State = 
    let ofMap (map: GardenMap) = 
        let start = GardenMap.getStart map
        { 
            Step = 0
            Positions = Set.singleton start
            Map = map 
        }
    
    let next (state: State) =
        let nextPositions = 
            state.Positions
            |> Seq.map (fun p -> GardenMap.possibleNextPosition p state.Map)
            |> Seq.concat
            |> Set.ofSeq
        {
            Step = state.Step + 1
            Positions = nextPositions
            Map = state.Map
        }

    let nextN (n: int) (state: State) =
        let rec loop n state =
            if n = 0 then state
            else loop (n - 1) (next state)
        loop n state

    let render (state: State) =
        for y in 0 .. state.Map.Height - 1 do
            for x in 0 .. state.Map.Width - 1 do
                let p = { X = x; Y = y }
                let c = 
                    if state.Positions |> Set.contains p then
                        PossibleLocation
                    else
                        state.Map.[x, y]
                printf "%c" c
            printfn ""


let sampleState = 
    sample
    |> GardenMap.parse
    |> State.ofMap

State.nextN 6 sampleState |> State.render

let wrapIndex i length =
    match i % length, sign i with
    | 0, _ -> 0
    | i, 1 -> i
    | i, -1 -> length + i
    | _, _ -> failwith "Impossible"

open System.Collections.Generic

type InfiniteState = 
    {
        Step: int
        Positions: HashSet<Point>
        Map: GardenMap
    }
    member this.Item with get (x, y) = 
        let x = wrapIndex x this.Map.Width
        let y = wrapIndex y this.Map.Height
        this.Map.[x, y]

module InfiniteState =
    let ofMap (map: GardenMap) = 
        let start = GardenMap.getStart map
        { 
            Step = 0
            Positions = new HashSet<_>([start])
            Map = map 
        }

    let possibleNextPosition { X = x; Y = y } (state: InfiniteState) (buffer: Point[]) = 
        let mutable count = 0
        match state.[x, y - 1] with
        | GardenPlot | Start -> 
            buffer[count] <- { X = x; Y = y - 1 }
            count <- count + 1
        | _ -> ()
        match state.[x, y + 1] with
        | GardenPlot | Start -> 
            buffer[count] <- { X = x; Y = y + 1 }
            count <- count + 1
        | _ -> ()
        match state.[x - 1, y] with
        | GardenPlot | Start -> 
            buffer[count] <- { X = x - 1; Y = y } 
            count <- count + 1
        | _ -> ()
        match state.[x + 1, y] with
        | GardenPlot | Start -> 
            buffer[count] <- { X = x + 1; Y = y } 
            count <- count + 1
        | _ -> ()
        count
    
    let next (state: InfiniteState) (buffer:Point[]) =
        let nextPositions = new HashSet<_>(state.Positions.Count)
        for p in state.Positions do
            let count = possibleNextPosition p state buffer
            for i in 0 .. count - 1 do
                nextPositions.Add(buffer.[i]) |> ignore
                
        {
            Step = state.Step + 1
            Positions = nextPositions
            Map = state.Map
        }

    let nextN (n: int) (state: InfiniteState) =
        let buffer = Array.zeroCreate<Point> 4
        let rec loop i state (buffer:Point[]) =
            if i = 0 then state
            else loop (i - 1) (next state buffer) buffer
        loop n state buffer


let sampleMap = GardenMap.parse sample
sampleMap
|> InfiniteState.ofMap
|> InfiniteState.nextN 500
|> (fun x -> 
    printfn "After 50 steps: %d" x.Positions.Count
    x.Positions.Count
)

let findStep (map: GardenMap) (step: int) =
    let period = map.Width
    let modulo = step % period
    let state = InfiniteState.ofMap map
    let buffer = Array.zeroCreate<Point> 4
    let steps = Array.zeroCreate<int> 500

    let rec loop (state: InfiniteState) =
        steps.[state.Step] <- state.Positions.Count
        if state.Step = (steps.Length - 1) then ()
        else
            let state1 = InfiniteState.next state buffer
            loop state1

    loop state

    let (x1, x2, x3) = 
        let x3 = (steps.Length / period) - 1
        let x2 = x3 - 1
        let x1 = x2 - 1
        (x1, x2, x3)

    let (y1, y2, y3) =
        let y1 = steps.[modulo + period * x1]
        let y2 = steps.[modulo + period * x2]
        let y3 = steps.[modulo + period * x3]
        (y1, y2, y3)
    let (y2', y3') = (y2 - y1, y3 - y2)

    let m = int64 (y3' - y2')
    let c =  (int64 (y3' + y2') - m * (int64 (x3 + x2)))/2L
    printfn "%d, %d" x1 y1
    printfn "%d, %d" x2 y2
    printfn "%d, %d" x3 y3
    printfn "y = %d x + %d" m c


    let nextStepCount knownStep knownCount nextStep =
        let n = int64 ((knownStep - modulo) / period)
        let nEnd = int64 ((nextStep - modulo) / period)

        let rec loop n steps =
            if n > nEnd then steps
            else
                let steps1 = steps + c + (n * m)
                loop (n + 1L) steps1

        loop n knownCount

    nextStepCount (modulo + period * x3) y2 step


let part2Sample = findStep sampleMap 5000

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__,"..", "input.txt"))

let gMap =
    input
    |> GardenMap.parse

let part1Answer = 
    input
    |> GardenMap.parse
    |> State.ofMap
    |> State.nextN 64
    |> (fun x -> 
        printfn "After 64 steps: %d" x.Positions.Count
        State.render x
        x.Positions.Count
    )

let part2Answer = findStep gMap 26501365
