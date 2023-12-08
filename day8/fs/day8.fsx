#r "nuget: FParsec, 1.1.1"

open System
open FParsec

type Node =
    {
        Name: string
    }

module Node = 
    // Name is 3 chars A-Z
    let pName = 
        manyChars (satisfy (fun c -> c >= 'A' && c <= 'Z'))
        >>= (fun chars -> 
            if chars.Length = 3 then
                preturn { Name = chars }
            else
                fail "Name must be 3 chars")

type Input =
    {
        Instructions: string
        Nodes: Map<Node, Node * Node>
    }

let sample = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""

module Input =
    let parse =
        let pInstructions = manyCharsTill (satisfy (fun c -> c = 'R' || c = 'L')) (newline .>> newline)
        let pLine = 
            parse {
                let! key = Node.pName
                let! _ = pstring " = ("
                let! left = Node.pName
                let! _ = pstring ", "
                let! right = Node.pName
                let! _ = pstring ")"
                let! _ = newline >>% () <|> eof
                return (key, (left, right))
            }
        let pLines = many pLine
        let pInput = pipe2 pInstructions pLines (fun instructions lines -> { Instructions = instructions; Nodes = Map.ofList lines })
        fun (input: string) ->
            match run pInput input with
            | Success (result, _, _) -> Some result
            | Failure (msg, _, _) -> 
                printfn "Error: %s" msg
                None

    let advance1 (node: Node) (input: Input) (steps: int) =
        match Map.tryFind node input.Nodes with
        | None -> failwithf "Node %s not found" node.Name
        | Some (left, right) ->
            match input.Instructions.[steps % input.Instructions.Length] with
            | 'R' -> right
            | 'L' -> left
            | _ -> failwith "Invalid instruction"

    let countSteps (input: Input) =
        let rec countSteps' (node: Node) (steps: int) =
            if node.Name = "ZZZ" then
                steps
            else
                let nextNode = advance1 node input steps
                countSteps' nextNode (steps + 1)

        countSteps' { Name = "AAA" } 0

    let countStepsUntil (node) atEnd (input: Input) =
        let rec countSteps' (node: Node) (steps: int) =
            if atEnd node steps then
                (node, steps)
            else
                let nextNode = advance1 node input steps
                countSteps' nextNode (steps + 1)

        countSteps' node 0


    let allAs (input: Input) =
        input.Nodes
        |> Map.keys
        |> Seq.filter _.Name.EndsWith('A')
        |> Array.ofSeq

    let allZs (nodes) =
        Array.TrueForAll(nodes, (fun s -> s.Name.EndsWith('Z')))

    let countSteps2 (nodes: Node []) (input: Input) =
        let rec countSteps' (nodes: Node []) (steps: int) =
            if steps % 1000 = 0 then
                printfn "%d" steps
            if allZs nodes then
                steps
            else
                let newNodes = nodes |> Array.Parallel.mapi (fun i node -> advance1 node input steps)
                countSteps' newNodes (steps + 1)

        countSteps' nodes 0

    let countStepsAll nodes (input: Input) =
        nodes |> Array.Parallel.map (fun node -> countStepsUntil node (fun node steps -> node.Name.EndsWith('Z') && steps > 0) input)

        

let sampleInput = Input.parse sample

let sampleSteps = sampleInput |> Option.map Input.countSteps 

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let parsedInput = input |> Input.parse |> Option.get

let steps = parsedInput |> Input.countSteps

let allSteps1 = parsedInput |> (fun x -> Input.countStepsAll (Input.allAs x) x)
let allSteps2 = parsedInput |>(fun x -> Input.countStepsAll (allSteps1 |> Array.map fst) x)
// Loops with common number of steps
let primeFactors x = 
    let rec primeFactors' x i =
        if x = 1L then
            []
        elif x % i = 0L then
            i :: primeFactors' (x / i) i
        else
            primeFactors' x (i + 1L)
    primeFactors' x 2L

let lowestCommonSteps = 
    allSteps2 
    |> (Array.map (snd >> int64 >> primeFactors))
    |> Seq.concat
    |> Set.ofSeq
    |> Seq.reduce ( * )
