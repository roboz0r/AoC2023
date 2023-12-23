open System
open System.Collections.Generic

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
type Pos = 
    { X: int; Y: int }
    override this.ToString() = sprintf "(%i,%i)" this.X this.Y

[<CustomEquality; NoComparison>]
type Edge = 
    {
        A: Pos
        B: Pos
        Between: Set<Pos>
    }
    member this.Length = this.Between.Count + 1
    override this.Equals (obj: obj) =
        match obj with
        | :? Edge as edge -> 
            ((this.A = edge.A && this.B = edge.B) 
                || (this.A = edge.B && this.B = edge.A))
            && this.Between = edge.Between
        | _ -> false

    override this.GetHashCode () =
        this.A.GetHashCode() + this.B.GetHashCode()

    override this.ToString () =
        sprintf "(%i,%i) <-> (%i,%i)" this.A.X this.A.Y this.B.X this.B.Y

type Node = Pos

module Node =
    let neighboursOnPath (input: Input) x y = 
        [ 
            { X = x; Y = y - 1 }
            { X = x + 1; Y = y }
            { X = x; Y = y + 1 }
            { X = x - 1; Y = y } 
        ]
        |> List.filter (fun pos -> pos.X >= 0 && pos.X < input.Width && pos.Y >= 0 && pos.Y < input.Height)
        |> List.filter (fun pos -> input.[pos.X, pos.Y] <> Forest)

    let findAllNodesInInput (input: Input) =
        let rec loop (x: int) (y: int) (nodes: Node list) =
            if y = input.Height then
                nodes
            elif x = input.Width then
                loop 0 (y + 1) nodes
            else
                match input.[x, y] with
                | Forest -> loop (x + 1) y nodes
                | _ ->
                    if y = 0 || y = input.Height - 1 then
                        let node = {  X = x; Y = y } 
                        loop (x + 1) y (node :: nodes)
                    else
                        let neighbours = neighboursOnPath input x y

                        match neighbours.Length with
                        | 3 | 4 ->
                            let node = { X = x; Y = y } 
                            loop (x + 1) y (node :: nodes)

                        | _ -> loop (x + 1) y nodes

        loop 0 0 []

    let rec findNextNode input (nodes: Set<Pos>) (edge: Edge) =
        if nodes.Contains edge.B then 
            edge
        else
            neighboursOnPath input edge.B.X edge.B.Y
            |> List.filter (fun pos -> pos <> edge.A && not (edge.Between.Contains pos))
            |> function
            | [ pos ] -> findNextNode input nodes { edge with B = pos; Between = edge.Between |> Set.add edge.B }
            | _ -> failwith "Invalid edge"

    let rec findEdges input (nodes: Node list) =
        let rec loop (head: Node) (tail: Node list) (edges: HashSet<Edge>) =
            neighboursOnPath input head.X head.Y
            |> List.map (fun pos -> 
                { A = head; B = pos; Between = Set.empty }
                |> findNextNode input (nodes |> Set.ofList)
            )
            |> List.iter (fun edge -> edges.Add(edge) |> ignore)
            match tail with
            | [] -> edges
            | head :: tail -> loop head tail edges

        match nodes with
        | [] -> HashSet<Edge>()
        | head :: tail -> loop head tail (HashSet<Edge>())




type Step =
    {
        Pos: Pos
        Edge: Edge
        Length: int
        Previous: Step list
        PreviousNodes: Node list
    }

module Step = 
    let start (edges: HashSet<Edge>) =
        edges
        |> Seq.find (fun edge -> edge.A.Y = 0 || edge.B.Y = 0)
        |> fun edge -> 
            if edge.A.Y = 0 then
                { Pos = edge.B; Edge = edge; Length = edge.Length; Previous = []; PreviousNodes = [edge.A] }
            else
                { Pos = edge.A; Edge = edge; Length = edge.Length; Previous = []; PreviousNodes = [edge.B] }

    let contains (step: Step) (edge: Edge) =
        step.Edge = edge || step.Previous |> List.exists (fun step -> step.Edge = edge)

    let containsNode (step: Step) (edge: Edge) =
        step.Edge = edge || step.PreviousNodes |> List.exists (fun node -> node = edge.A || node = edge.B)

    let append (step: Step) (edge: Edge) =
        let length = step.Length + edge.Length
        let previous = step :: step.Previous
        let previousNodes = step.Pos :: step.PreviousNodes
        let pos = 
            if step.Pos = edge.A then
                edge.B
            else
                edge.A
        { 
            Pos = pos
            Edge = edge
            Length = length
            Previous = previous 
            PreviousNodes = previousNodes
        }
    let print (step: Step) =
        let rec loop (step: Step) =
            match step.Previous with
            | [] -> printfn "%i,%i" step.Edge.A.X step.Edge.A.Y
            | step :: _ -> loop step; printfn "%i,%i" step.Edge.B.X step.Edge.B.Y
        loop step

type State =
    {
        Start: Edge
        Frontier: Step list
        Completed: Step option
    }


module State =

    let addCompleted (input:Input) (state: State) (step: Step) =
        if step.Edge.A.Y = input.Height - 1 || step.Edge.B.Y = input.Height - 1 then
            match state.Completed with
            | Some completed -> 
                if step.Length > completed.Length then
                    printfn "Completed %i" step.Length
                    Some step
                else
                    state.Completed
            | None -> 
                printfn "Completed %i" step.Length
                Some step
        else
            state.Completed

    let nextSteps (edges: Map<Node, Edge list>) (step: Step) =
        // printfn "At %O" step.Pos
        edges.TryFind step.Pos 
        |> Option.defaultValue []
        |> List.filter (fun edge -> 
            // printfn "%O" edge
            not (Step.containsNode step edge)
        )
        |> List.map (fun edge -> Step.append step edge)

    let hike edges input (state: State) =
        let rec loop edges (state: State) (step: Step) =
            // Step.print step
            match nextSteps edges step with
            | [] -> 
                let completed = addCompleted input state step
                
                match state.Frontier with
                | [] -> { state with Completed = completed }
                | step :: frontier -> loop edges { state with Frontier = frontier; Completed = completed } step
            | step :: others ->
                // let edges = edges.Remove(step.Pos)
                let frontier = others @ state.Frontier
                loop edges { state with Frontier = frontier } step

        loop edges state state.Frontier.Head


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

let sampleNodes = Node.findAllNodesInInput parsedSample
let sampleEdges = Node.findEdges parsedSample sampleNodes

let createEdgeMapping (edges: Edge list) =
    let addIfNew edge edges =
        match edges |> List.contains edge with
        | true -> edges
        | false -> edge :: edges


    let rec loop (edges: Edge list) (mapping: Map<Node, Edge list>) =
        match edges with
        | [] -> mapping
        | edge :: edges ->
            let mapping = 
                match mapping.TryGetValue edge.A with
                | true, edges -> mapping |> Map.add edge.A (addIfNew edge edges)
                | false, _ -> mapping |> Map.add edge.A [edge]
            let mapping = 
                match mapping.TryGetValue edge.B with
                | true, edges -> mapping |> Map.add edge.B (addIfNew edge edges)
                | false, _ -> mapping |> Map.add edge.B [edge]
            loop edges mapping
    loop edges Map.empty




(createEdgeMapping (List.ofSeq sampleEdges))
|> Map.iter (fun node edges -> printfn "%O -> %O" node edges)

let hikeSample =
    let start = Step.start sampleEdges
    let state = { Start = start.Edge; Frontier = [ start ]; Completed = None }
    State.hike (createEdgeMapping (List.ofSeq sampleEdges)) parsedSample state

let longestPath (input:string) =
    let parsed = Input.parse input
    let nodes = Node.findAllNodesInInput parsed
    let edges = Node.findEdges parsed nodes
    let start = Step.start edges
    let state = { Start = start.Edge; Frontier = [ start ]; Completed = None }
    let state = State.hike (createEdgeMapping (List.ofSeq edges)) parsed state
    state.Completed.Value.Length

let longestPathSample = longestPath sample


open System.IO
#time "on"
let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))
let longestPathInput = longestPath input
#time "off"

