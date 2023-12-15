open System

let day15Hash (s:string) =
    let rec loop (i:int) (hash:byte) =
        if i = s.Length then hash
        else 
            match s[i] with
            | '\r' | '\n' -> loop (i+1) hash
            | c ->
                loop (i+1) ((hash + (byte c))* 17uy)
    loop 0 0uy

let [<Literal>] RemoveLens = '-'
let [<Literal>] AddLens = '='

type Op =
    | Add of string * int
    | Remove of string

let parseOp (s:string) =
    match s.IndexOf(RemoveLens) with
    | -1 -> 
        let parts = s.Split(AddLens)
        Add(parts.[0], int parts.[1])
    | i ->
        let label = s.Substring(0, i)
        Remove(label)

open System.IO

type Lens =
    {
        Label: string
        FocalLength: int
    }

type Day15HashMap () =
    let arr = Array.create<Lens list> 256 []

    let rec addOrReplace newLens (acc:Lens list):Lens list =
        match acc with
        | [] -> [newLens]
        | l::ls when l.Label = newLens.Label -> newLens::ls
        | l::ls -> l::addOrReplace newLens ls

    member _.Add(label, focalLength) =
        let hash = day15Hash label
        let idx = int hash
        let l = {Label = label; FocalLength = focalLength}
        arr.[idx] <- addOrReplace l arr.[idx]

    member _.Remove(label) =
        let hash = day15Hash label
        let idx = int hash
        arr.[idx] <- arr.[idx] |> List.filter (fun l -> l.Label <> label)

    override _.ToString() =
        arr
        |> Array.mapi (fun i l -> i, l)
        |> Array.filter (fun (_, l) -> l <> [])
        |> Array.map (fun (i, l) -> 
            let lenses = String.Join(' ', l |> List.map (fun l -> $"[{l.Label} {l.FocalLength}]"))
            i, lenses)
        
        |> Array.map (fun (i, l) -> $"Box {i}: {l}")
        |> fun x -> String.Join("\n", x)

    member _.FocalPower =
        arr
        |> Array.mapi (fun i l -> 
            let i = (i + 1)
            l |> List.mapi (fun j l -> 
                let j = (j + 1)
                i * j * l.FocalLength)
            |> List.sum
        )
        |> Array.sum

day15Hash "HASH"

let x = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
// Part 1 Sample
x.Split(',')
|> Array.map day15Hash

// Part 2 Sample
let map = new Day15HashMap()
x.Split(',')
|> Array.map parseOp
|> Array.iter (fun op ->
    match op with
    | Add(label, focalLength) -> 
        printfn $"""After "{label}={focalLength}" """
        map.Add(label, focalLength)
        printfn "%O\n" map
    | Remove(label) -> 
        printfn $"""After "{label}-" """
        map.Remove(label)
        printfn "%O\n" map
)
map.FocalPower

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))
let inputSplit = input.Split(',')
// Part 1
inputSplit
|> Array.map (day15Hash >> int)
|> Array.sum

//Part 2
let map2 = new Day15HashMap()
inputSplit
|> Array.map parseOp
|> Array.iter (fun op ->
    match op with
    | Add(label, focalLength) -> 
        map2.Add(label, focalLength)
    | Remove(label) -> 
        map2.Remove(label)
)
map2.FocalPower