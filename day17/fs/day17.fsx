open System
open LanguagePrimitives

type [<Measure>] heat
type HeatMap =
    {
        Rows: string[]
    }
    member this.Width = this.Rows.[0].Length
    member this.Height = this.Rows.Length
    member this.Item with get (x, y) = 
        Int32WithMeasure<heat>(int (this.Rows.[y].[x]) - int '0')

module HeatMap = 
    let parse (input: string) =
        let rows = input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        {
            Rows = rows
        }

type Direction =
    | Up
    | Down
    | Left
    | Right
    member this.Value =
        match this with
        | Up -> 0
        | Down -> 1
        | Left -> 2
        | Right -> 3
    member this.Char =
        match this with
        | Up -> '^'
        | Down -> 'v'
        | Left -> '<'
        | Right -> '>'

    static member All = [Up; Down; Left; Right]

// Try converting the input into a lazily evaluated graph. 
// Advance through the graph only on the lowest node. 
// This will allow us to keep track of the current position of the crucible.
// When the crucible reaches the bottom-right, we can calculate the score.
// The first was reached must be the lowest node?
// Data structure?

// Move history can be stored as a list (stack)
type Point = 
    {
        X: int
        Y: int
    }


type StepKey =
    {
        X: int
        Y: int
        Direction: Direction
        Consecutive: int
    }
type Step =
    {
        X: int
        Y: int
        Direction: Direction
        Consecutive: int
        HeatLoss: int<heat>
    }
    static member Start(heatMap: HeatMap, x, y) =
        {
            X = x
            Y = y
            Direction = Right
            Consecutive = 0
            HeatLoss = 0<heat>
        }
    member this.Key =
        {
            X = this.X
            Y = this.Y
            Direction = this.Direction
            Consecutive = this.Consecutive
        }

module Step =
    let tryMove (heatMap: HeatMap) direction step =
        match direction with
        | Up -> 
            match step.Direction with
            | Up -> Some (step.X, step.Y - 1, step.Consecutive + 1)
            | Down -> None
            | _ -> Some (step.X, step.Y - 1, 1)
        | Down -> 
            match step.Direction with
            | Up -> None
            | Down -> Some (step.X, step.Y + 1, step.Consecutive + 1)
            | _ -> Some (step.X, step.Y + 1, 1)
        | Left -> 
            match step.Direction with
            | Left -> Some (step.X - 1, step.Y, step.Consecutive + 1)
            | Right -> None
            | _ -> Some (step.X - 1, step.Y, 1)
        | Right -> 
            match step.Direction with
            | Left -> None
            | Right -> Some (step.X + 1, step.Y, step.Consecutive + 1)
            | _ -> Some (step.X + 1, step.Y, 1)
        |> Option.bind (fun (x, y, consecutive) ->

            if x < 0 || x >= heatMap.Width || y < 0 || y >= heatMap.Height || consecutive >= 4 then
                None
            else
                Some ({
                    X = x
                    Y = y
                    Consecutive = consecutive
                    Direction = direction
                    HeatLoss = step.HeatLoss + heatMap.[x, y]
                })
        )
    let tryMove2 (heatMap: HeatMap) direction step =
        // Minimum of 4
        // Maximum of 10
        match direction with
        | Up -> 
            match step.Direction with
            | Up -> Some (step.X, step.Y - 1, step.Consecutive + 1)
            | Down -> None
            | _ -> 
                if step.Consecutive >= 4 then
                    Some (step.X, step.Y - 1, 1)
                else None
        | Down -> 
            match step.Direction with
            | Up -> None
            | Down -> Some (step.X, step.Y + 1, step.Consecutive + 1)
            | _ -> 
                if step.Consecutive >= 4 then
                    Some (step.X, step.Y + 1, 1)
                else None
        | Left -> 
            match step.Direction with
            | Left -> Some (step.X - 1, step.Y, step.Consecutive + 1)
            | Right -> None
            | _ ->
                if step.Consecutive >= 4 then
                    Some (step.X - 1, step.Y, 1)
                else None
        | Right -> 
            match step.Direction with
            | Left -> None
            | Right -> Some (step.X + 1, step.Y, step.Consecutive + 1)
            | _ -> 
                if step.Consecutive >= 4 then
                    Some (step.X + 1, step.Y, 1)
                else None
        |> Option.bind (fun (x, y, consecutive) ->

            if x < 0 || x >= heatMap.Width || y < 0 || y >= heatMap.Height || consecutive > 10 then
                None
            else
                Some ({
                    X = x
                    Y = y
                    Consecutive = consecutive
                    Direction = direction
                    HeatLoss = step.HeatLoss + heatMap.[x, y]
                })
        )

// Rewrite algorithm to use https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
// procedure uniform_cost_search(start) is
//     node ← start
//     frontier ← priority queue containing node only
//     expanded ← empty set
//     do
//         if frontier is empty then
//             return failure
//         node ← frontier.pop()
//         if node is a goal state then
//             return solution(node)
//         expanded.add(node)
//         for each of node's neighbors n do
//             if n is not in expanded and not in frontier then
//                 frontier.add(n)
//             else if n is in frontier with higher cost
//                 replace existing node with n

// Looks like I'm only missing the "higher cost" part of the algorithm.

type Frontier =
    {
        Priority: Map<int<heat>, Step list list>
        Index: Map<StepKey, Step list>
    }

module Frontier =
    let pop (frontier: Frontier) =
        let (KeyValue(loss, steps)) = Seq.head frontier.Priority
        match steps with
        | [] -> failwith "Empty steps"
        | steps :: rest ->
            let priority = 
                match rest with
                | [] ->frontier.Priority |> Map.remove loss
                | _ -> frontier.Priority |> Map.add loss rest
            let head = steps.Head
            let index = frontier.Index |> Map.remove head.Key
            steps, { Priority = priority; Index = index }

    let addOrReplaceIfLower (newSteps: Step list) (frontier: Frontier) =
        match newSteps with
        | [] -> failwith "Empty steps"
        | head :: _ ->
            let loss = head.HeatLoss
            let key = head.Key
            match frontier.Index |> Map.tryFind key with
            | Some steps ->
                // In index so just only add if it's a lower loss
                if loss < steps.Head.HeatLoss then
                    let priority = 
                        match frontier.Priority |> Map.tryFind loss with
                        | Some steps -> frontier.Priority |> Map.add loss (newSteps :: steps)
                        | None -> frontier.Priority |> Map.add loss [newSteps]
                    let priority = 
                        let existingSteps = 
                            priority 
                            |> Map.find steps.Head.HeatLoss
                            |> List.filter (fun s -> s <> steps)
                        match existingSteps with
                        | [] -> priority |> Map.remove steps.Head.HeatLoss
                        | _ -> priority |> Map.add steps.Head.HeatLoss existingSteps
                    let index = frontier.Index |> Map.add key newSteps
                    { Priority = priority; Index = index }

                else
                    // In the index but not a lower loss so just return the existing
                    frontier
            | None ->
                // Not in index so just add it
                let steps = 
                    match frontier.Priority |> Map.tryFind loss with
                    | Some steps -> newSteps :: steps
                    | None -> [newSteps]
                {
                    Priority = frontier.Priority |> Map.add loss steps
                    Index = frontier.Index |> Map.add key newSteps
                }

type State = 
    {
        Frontier: Frontier
        Visited: Set<Point>
    }
module State =
    let pop (state: State) =
        let steps, frontier = state.Frontier |> Frontier.pop
        steps, { state with Frontier = frontier }

    let addOrReplaceIfLower (newSteps: Step list) (state: State) =
        let frontier = state.Frontier |> Frontier.addOrReplaceIfLower newSteps
        { Frontier = frontier; Visited = state.Visited |> Set.add { X = newSteps.Head.X; Y = newSteps.Head.Y } }

    let atEnd (heatMap: HeatMap) (steps: Step list) =
        match steps with
        | step :: _ ->  step.X = heatMap.Width - 1 && step.Y = heatMap.Height - 1
        | _ -> false

    let advance tryMove heatMap state =
        let steps, state = pop state
        if atEnd heatMap steps then
            Some steps, state
        else
            let state = 
                Direction.All |> List.choose (fun d -> 
                    let newStep = tryMove heatMap d steps.Head
                    newStep |> Option.map (fun step -> step :: steps)
                ) |> List.fold (fun state steps -> state |> addOrReplaceIfLower steps) state
            None, state


    let findEnd heatMap state =
        let rec loop state =
            match advance Step.tryMove heatMap state with
            | Some steps, _ -> steps, state
            | None, state -> loop state
        loop state
    let findEnd2 heatMap state =
        let rec loop state =
            match advance Step.tryMove2 heatMap state with
            | Some steps, _ -> steps, state
            | None, state -> loop state
        loop state

    let start heatMap x y = 
        let point = { X = x; Y = y }
        let step = Step.Start(heatMap, x, y)
        {
            Frontier = {
                Priority = Map.empty |> Map.add step.HeatLoss [[step]]
                Index = Map.empty |> Map.add step.Key [step]
            }
            Visited = Set.singleton point
        }

let sample = """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"""

let parsedSample = HeatMap.parse sample
parsedSample.Height
#time "on"
let state = State.start parsedSample 0 0

let steps, endState = State.findEnd parsedSample state
steps |> List.rev |> List.iter (fun x -> printfn "%A" (x.Direction, x.HeatLoss))
endState.Frontier.Priority |> Map.iter (fun loss steps -> printfn $"{loss}: {steps.Length}")
// endState.Visited.Count |> Set.iter (fun p -> printfn $"{p.X}, {p.Y}")
steps |> List.rev |> List.map (fun s -> s.Direction) |> printfn "%A"
#time "off"

let moves =
    [
        Right
        Right
        Down
        Right
        Right
        Right
        Up
        Right
        Right
        Right
        Down
        Down
        Right
        Right
        Down
        Down
        Right
        Down
        Down
        Down
        Right
        Down
        Down
        Down
        Left
        Down
        Down
        Right
    ]

// ((Some (Step.Start(parsedSample, 0, 0))), moves)
// ||> List.fold (fun state direction -> 
//     state 
//     |> Option.bind (Step.tryMove parsedSample direction)
//     ) 
// |> Option.bind (fun step -> State.atEnd parsedSample (Map.empty |> State.add [step]))

open System.IO 

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))


#time "on"
let parsedInput = HeatMap.parse input
let state0 = State.start parsedInput 0 0

// let steps0, endState0 = State.findEnd parsedInput state0
let steps2, endState2 = State.findEnd2 parsedInput state0
printfn "%A" steps2.Head
#time "off"
