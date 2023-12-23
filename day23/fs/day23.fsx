open System

let [<Literal>] Path = '.'
let [<Literal>] Forest = '#'
let [<Literal>] SlopeUp = '^'
let [<Literal>] SlopeRight = '>'
let [<Literal>] SlopeDown = 'v'
let [<Literal>] SlopeLeft = '<'
let [<Literal>] Start = 'S'
let [<Literal>] Hiked = 'O'

type Input =
    {
        Map: string []
    }
    member this.Item with get (x, y) = this.Map.[y].[x]
    member this.Width = this.Map.[0].Length
    member this.Height = this.Map.Length

module Input =
    let parse (s: string) =
        let map = s.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        { Map = map }

// start on a singe path in the top row
// can only go downhill on a slope
// never go to the same tile twice
// what is the longest possible path

[<Struct>]
type Pos = { X: int; Y: int }

type Step =
    {
        Pos: Pos
        Length: int
        Previous: Step list
        PreviousSet: Set<Pos>
    }

type State =
    {
        Map: string []
        Start: Pos
        Frontier: Step list
        Completed: Map<int, Step list>
    }
    member this.Item with get (x, y) = this.Map.[y].[x]
    member this.Width = this.Map.[0].Length
    member this.Height = this.Map.Length

module Step = 
    let start (input: Input) =
        let x = input.Map.[0].IndexOf(Path)
        {
            Pos = { X = x; Y = 0 }
            Length = 0
            Previous = []
            PreviousSet = Set.empty
        }

    let isPossible (state: State) (step: Step) (next: Pos) =
        if next.X < 0 || next.X >= state.Width || next.Y < 0 || next.Y >= state.Height then
            false
        elif Set.contains next step.PreviousSet then
            false
        else
            match state.[next.X, next.Y] with
            | Forest -> false
            | _ -> true

    let nextSteps (state: State) (step: Step) =
        let x = step.Pos.X
        let y = step.Pos.Y
        let length = step.Length + 1
        let previous = step :: step.Previous
        let previousSet = Set.add step.Pos step.PreviousSet

        match state.[x, y] with
        | SlopeUp -> [ { X = x; Y = y - 1 } ]
        | SlopeRight -> [ { X = x + 1; Y = y } ]
        | SlopeDown -> [ { X = x; Y = y + 1 } ]
        | SlopeLeft -> [ { X = x - 1; Y = y } ]
        | _ ->
            [ { X = x; Y = y - 1 }
              { X = x + 1; Y = y }
              { X = x; Y = y + 1 }
              { X = x - 1; Y = y } ]
        |> List.filter (isPossible state step)
        |> List.map (fun pos -> { Pos = pos; Length = length; Previous = previous; PreviousSet = previousSet })

module State =
    let ofInput (x: Input) =
        let start = Step.start x
        {
            Map = x.Map
            Start = start.Pos
            Frontier = [ start ]
            Completed = Map.empty
        }

    let addCompleted (state: State) (step: Step) =
        if step.Pos.Y = state.Height - 1 then
            match state.Completed.TryGetValue step.Length with
            | true, steps -> Map.add step.Length (step :: steps) state.Completed
            | false, _ -> Map.add step.Length [step] state.Completed
        else
            state.Completed

    let hike (state: State) =
        let rec loop (state: State) (step: Step) =
            match Step.nextSteps state step with
            | [] -> 
                let completed = addCompleted state step
                
                match state.Frontier with
                | [] -> { state with Completed = completed }
                | step :: frontier -> loop { state with Frontier = frontier; Completed = completed } step
            | step :: others ->
                let frontier = others @ state.Frontier
                loop { state with Frontier = frontier } step

        loop state state.Frontier.Head

    let print (state: State) (step: Step) =
        for y in 0 .. state.Height - 1 do
            for x in 0 .. state.Width - 1 do
                let pos = { X = x; Y = y }
                if pos = state.Start then
                    printf $"{Start}"
                elif Set.contains pos step.PreviousSet then
                    printf $"{Hiked}"
                else
                    printf $"{state.[x, y]}"
            printfn ""
        printfn $"Length: {step.Length}"

    let printLongest (state: State) =
        let step = state.Completed |> Seq.maxBy (fun (KeyValue(k, _)) -> k) |> (fun (KeyValue(_, v)) -> List.head v)
        print state step

let sample = """#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#"""

let parsedSample = Input.parse sample

let stateSample = State.ofInput parsedSample

let hiked = State.hike stateSample

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))
let parsedInput = Input.parse input
let stateInput = State.ofInput parsedInput

let part1Answer = 
    State.hike stateInput
    |> (fun x -> x.Completed |> Seq.maxBy (fun (KeyValue(k, _)) -> k) |> (fun (KeyValue(k, _)) -> k))

printfn $"Part 1: {part1Answer}"

