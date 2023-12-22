#r "nuget: FParsec, 1.1.1"

open System
open FParsec

let sample = """1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
"""

type Point =
    {
        X: int
        Y: int
        Z: int
    }
    override this.ToString() =
        $"{this.X},{this.Y},{this.Z}"

// Ground at Z=0
// Lowest Z of a brick is 1

type InputBrick =
    {
        StartPt: Point
        EndPt: Point
    }

[<Measure>] type brick

let toLetters (i: int<brick>): string =
    let a = int 'A'
    let z = int 'Z'
    let n = z - a + 1
    let rec toLetters' (i: int) (acc: string) =
        if i = 0 then acc
        else
            let i = i - 1
            let c = char (a + (i % n))
            let acc = string c + acc
            toLetters' (i / n) acc
    toLetters' ((int i) + 1) ""

toLetters 0<brick>

type Brick =
    {
        Id: int<brick>
        Start: Point
        End: Point
    }
    member this.Volume =
        let dx = (this.End.X - this.Start.X) + 1
        let dy = (this.End.Y - this.Start.Y) + 1
        let dz = (this.End.Z - this.Start.Z) + 1
        dx * dy * dz

    override this.ToString() =
        $"{this.Start}~{this.End}   <- {toLetters this.Id}"

module Brick =
    let translateZ (z: int) (brick: Brick) =
        { brick with Start = { brick.Start with Z = brick.Start.Z + z }; End = { brick.End with Z = brick.End.Z + z } }

    
    let occupies (brick: Brick) (point: Point) =
        let x = point.X
        let y = point.Y
        let z = point.Z
        let startX = brick.Start.X
        let startY = brick.Start.Y
        let startZ = brick.Start.Z
        let endX = brick.End.X
        let endY = brick.End.Y
        let endZ = brick.End.Z
        x >= startX && x <= endX && y >= startY && y <= endY && z >= startZ && z <= endZ

    let bottomSurface (brick: Brick) =
        [| for x in brick.Start.X .. brick.End.X do
            for y in brick.Start.Y .. brick.End.Y do
                { X = x; Y = y; Z = brick.Start.Z } |]

    let inOrder (brick: Brick) =
        let start = brick.Start
        let end' = brick.End
        let x = start.X <= end'.X
        let y = start.Y <= end'.Y
        let z = start.Z <= end'.Z
        x && y && z

type BrickStack =
    {
        Bricks: Brick list
    }

type SettledBrickStack =
    {
        Supports: Map<Brick, Brick list>
        SupportedBy: Map<Brick, Brick list>
        Bricks: Brick list
    }

module BrickStack = 
    let pPoint: Parser<Point, unit> =
        pipe3
            (pint32 .>> pchar ',')
            (pint32 .>> pchar ',')
            (pint32)
            (fun x y z -> { X = x; Y = y; Z = z })

    let pBrick: Parser<InputBrick, unit> =
        pipe2
            (pPoint .>> pchar '~')
            (pPoint)
            (fun start end' -> { StartPt = start; EndPt = end' })

    let pBrickStack: Parser<BrickStack, unit> =
        many1Till (pBrick .>> newline) eof
        |>> fun bricks -> 
            { 
                Bricks = 
                    bricks 
                    |> List.mapi (fun i brick -> 
                        { Id = i * 1<brick>; Start = brick.StartPt; End = brick.EndPt })
            }

    let tryParse (s: string) =
        match run pBrickStack s with
        | Success(result, _, _) -> Some(result)
        | Failure(_, _, _) -> None

    let tryDropOne (brick: Brick) (bricksBelow: Brick list) =
        if brick.Start.Z = 1 then 
            None, [] // Brick on ground
        else
            let brick1 = Brick.translateZ -1 brick
            let surface1 = Brick.bottomSurface brick1
            let anyIntersecting b =
                surface1 |> Array.fold (fun any p -> any || Brick.occupies b p) false

            let anySupporting = 
                (bricksBelow |> List.filter anyIntersecting)

            match anySupporting with
            | [] -> Some brick1, []
            | _ -> None, anySupporting

    let supports (supportedBy: Map<Brick, Brick list>) =
        let s = Map.empty
        let loop (s: Map<Brick, Brick list>) b suppBy =
            suppBy |> List.fold (fun s b' ->
                match Map.tryFind b' s with
                | None -> Map.add b' [b] s
                | Some(bs) -> Map.add b' (b :: bs) s) s

        supportedBy |> Map.fold loop s
            
    let settle (x: BrickStack) =
        let supportedBy: Map<Brick, Brick list> = Map.empty // Brick is supported by these bricks
        let bricks = x.Bricks |> List.sortBy (fun brick -> brick.Start.Z)
        let rec settle' (brick: Brick) (remaining: Brick list) (output: Brick list) (supportedBy: Map<Brick, Brick list>)  =
            match tryDropOne brick output with
            | Some(brick1), _ ->
                settle' brick1 remaining output supportedBy
            | None, supportedByBs ->
                let supportedBy = supportedBy |> Map.add brick supportedByBs
                match remaining with
                | [] -> (brick :: output), supportedBy
                | brick1 :: remaining1 ->
                    settle' brick1 remaining1 (brick :: output) supportedBy

        let bricks, supportedBy = 
            match bricks with
            | [] -> [], supportedBy
            | brick :: remaining ->
                settle' brick remaining [] supportedBy
        { 
            Supports = supports supportedBy
            SupportedBy = supportedBy
            Bricks = bricks 
        }

    let print (x: BrickStack) =
        let rec print' (x: Brick list) =
            match x with
            | [] -> ()
            | brick :: bricks ->
                printfn $"{brick}"
                print' bricks
        print' x.Bricks

module SettledBrickStack =

    let onGroundOrSupportedBy2 (x: SettledBrickStack) (b: Brick) =
        b.Start.Z = 1 || 
        match x.SupportedBy.[b] with
        | [] -> failwith "Brick should be supported by something"
        | [b'] -> false
        | _ -> true


    let listDisintegratable (x: SettledBrickStack): Brick list =
        x.Bricks
        |> List.filter (fun b -> 
            match x.Supports|> Map.tryFind b with
            | None -> true
            | Some bs ->
                bs
                |> List.filter (onGroundOrSupportedBy2 x >> not)
                |> List.isEmpty
            
            )

    let chainFallCount (x: SettledBrickStack) (b: Brick) =
        let oldBricks = x.Bricks
        let removed = x.Bricks |> List.filter (fun b' -> b' <> b)
        let resettled = 
            BrickStack.settle { Bricks = removed }
            // |> fun x -> (BrickStack.settle { Bricks = x.Bricks })
        let newBricks = resettled.Bricks

        newBricks |> List.sumBy (fun b' -> 
            if oldBricks |> List.contains b' then 0 else 1)



let sampleBrickStack = 
    match BrickStack.tryParse sample with
    | Some(brickStack) -> brickStack
    | None -> failwith "Failed to parse"

BrickStack.print sampleBrickStack

let sampleSettledBrickStack = BrickStack.settle sampleBrickStack

SettledBrickStack.listDisintegratable sampleSettledBrickStack |> List.length

sampleSettledBrickStack.Supports |> Map.iter (fun b bs ->
    printfn $"{toLetters b.Id} supports {String.Join(',', bs |> List.map (fun x -> toLetters x.Id))} bricks"
)

sampleSettledBrickStack.SupportedBy |> Map.iter (fun b bs ->
    printfn $"{toLetters b.Id} supported by {String.Join(',', bs |> List.map (fun x -> toLetters x.Id))} bricks"
)

sampleSettledBrickStack.Bricks 
|> List.filter (SettledBrickStack.onGroundOrSupportedBy2 sampleSettledBrickStack) 
|> List.map (fun b -> toLetters b.Id) |> (fun x -> String.Join(',', x))

sampleBrickStack.Bricks |> List.map (printfn "%O")
sampleSettledBrickStack.Bricks |> List.map (printfn "%O")

sampleSettledBrickStack.Bricks
|> List.filter (fun b -> 
    match sampleSettledBrickStack.Supports|> Map.tryFind b with
    | None -> true
    | Some bs ->
        bs
        |> List.filter (SettledBrickStack.onGroundOrSupportedBy2 sampleSettledBrickStack >> not)
        |> List.isEmpty
    
    )
|> List.iter (fun b -> printfn "%O" b)


sampleSettledBrickStack.Bricks
|> List.map (fun b -> b, SettledBrickStack.chainFallCount sampleSettledBrickStack b)
|> List.iter (fun (b, i) -> printfn "%O, %i" b i)

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let parsedInput = 
    match BrickStack.tryParse input with
    | Some(brickStack) -> brickStack
    | None -> failwith "Failed to parse"

parsedInput.Bricks |> List.fold (fun acc brick -> acc && Brick.inOrder brick) true

let settledInput = 
    parsedInput
    |> BrickStack.settle

// Part 1 = 534
let part1Answer =
    settledInput
    |> SettledBrickStack.listDisintegratable
    |> List.length


let part2Answer = 
    settledInput.Bricks
    |> List.sumBy (fun b -> SettledBrickStack.chainFallCount settledInput b)

