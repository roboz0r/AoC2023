open System
open System.Text

let [<Literal>] Operational = '.'
let [<Literal>] Damaged = '#'
let [<Literal>] Unknown = '?'

let sample = """#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1"""


type ConditionRecord = 
    {
        Record: string
        DamagedGroups: int list
    }

module ConditionRecord =
    let parse (input: string) = 
        let parts = input.Split([|' '|])
        if parts.Length <> 2 then
            failwithf "Invalid input: %s" input

        let record = parts.[0]
        let damagedGroups = 
            parts.[1].Split([|','|])
            |> Array.map int
            |> List.ofArray
        {
            Record = record
            DamagedGroups = damagedGroups
        }

    let parseAll (input: string) = 
        input.Split([|'\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parse

    let patterns = Array.init 16 (fun i -> 
        // max value in input is 15
        if i = 0 then
            null // 0 is invalid
        else
            (StringBuilder())
                .Append(Operational)
                .Append(Damaged, i)
                .Append(Operational)
                .ToString()
    )

    let isPossible (record: ReadOnlySpan<char>) (pattern: ReadOnlySpan<char>) =
        let mutable i = 0
        let mutable possible = true
        while possible && i < pattern.Length && i < record.Length do
            match record.[i], pattern.[i] with
            | Operational, Operational
            | Damaged, Damaged
            | Unknown, Operational
            | Unknown, Damaged -> 
                i <- i + 1
            | _ -> possible <- false
        possible

    let possibleContiguous (record: string) (contiguous: int) =
        let pattern = patterns.[contiguous]
        let rec loop i positions =
            if i = -1 then 
                // Consider leading operational as off-edge
                if isPossible (record.AsSpan()) (pattern.AsSpan().Slice(1)) then
                    let i = i + 1
                    loop i (i :: positions)
                else
                    loop (i + 1) positions
            elif i = record.Length - pattern.Length + 1 then
                // Consider trailing operational as off-edge
                if isPossible (record.AsSpan().Slice(i)) (pattern.AsSpan().Slice(0, pattern.Length - 1)) then
                    let i = i + 1
                    List.rev (i :: positions)
                else
                    List.rev positions
            else
                if isPossible (record.AsSpan().Slice(i)) (pattern.AsSpan()) then
                    let i = i + 1
                    loop i (i :: positions)
                else
                    loop (i + 1) positions

        loop (-1) []

    let possibleContiguous2 (record: string) (contiguous: int) min = // max =
        let pattern = patterns.[contiguous]
        let rec loop i positions =
            // if i > max then
            //     List.rev positions
            if i = -1 then 
                // Consider leading operational as off-edge
                if isPossible (record.AsSpan()) (pattern.AsSpan().Slice(1)) then
                    let i = i + 1
                    loop i (i :: positions)
                else
                    loop (i + 1) positions
            elif i = record.Length - pattern.Length + 1 then
                // Consider trailing operational as off-edge
                if isPossible (record.AsSpan().Slice(i)) (pattern.AsSpan().Slice(0, pattern.Length - 1)) then
                    let i = i + 1
                    List.rev (i :: positions)
                else
                    List.rev positions
            else
                if isPossible (record.AsSpan().Slice(i)) (pattern.AsSpan()) then
                    let i = i + 1
                    loop i (i :: positions)
                else
                    loop (i + 1) positions

        loop min []

    let possiblePairs head combinations contiguous =
        combinations
        |> List.filter (fun combination -> combination > (head + contiguous))

    let renderCombination (x: ConditionRecord) (combination: int list) =
        let combination = List.rev combination
        let arr = Array.init x.Record.Length (fun _ -> Operational)
        let mutable i = 0
        for y in combination do
            let dmg = x.DamagedGroups.[i]
            for j in y .. (y + dmg - 1) do
                arr.[j] <- Damaged
            i <- i + 1
        arr

    let isValid (x: ConditionRecord) (case :char[]) =
        if case.Length <> x.Record.Length then
            failwithf "Invalid case: %A" case

        let mutable i = 0
        let mutable valid = true
        while valid && i < case.Length do
            match x.Record.[i], case.[i] with
            | Operational, Operational
            | Damaged, Damaged
            | Unknown, Operational
            | Unknown, Damaged -> 
                i <- i + 1
            | _ -> valid <- false
        
        valid

    let getWindows (record: ConditionRecord) =
        let groups = Array.ofList record.DamagedGroups
        let mutable max = record.Record.Length - (Array.sum groups) - 1
        let mutable min = -1
        Array.init groups.Length (fun i ->
            let g = groups.[i]
            let x = g, min, max
            min <- min + g + 1
            max <- max - g - 1
            x
        )

    let allPossibleContiguous (record: ConditionRecord) = //: int list list =
        // let potentialCombinations =
        //     record.DamagedGroups
        //     |> List.map (possibleContiguous record.Record)

        let potentialCombinations =
            [
                let mutable min = -1
                for i in record.DamagedGroups do 
                    let xs = possibleContiguous record.Record i
                    let xs = xs |> List.filter (fun x -> x > min)
                    min <- xs |> List.tryHead |> Option.defaultValue min
                    xs
            ]
            |> List.rev

        let potentialCombinations =
            [
                let mutable max = record.Record.Length
                for x in potentialCombinations do
                    let xs = x |> List.filter (fun x -> x < max)
                    max <- xs |> List.tryLast |> Option.defaultValue max
                    xs
            ]
            |> List.rev
            
        // let potentialCombinations = 
        //     record.DamagedGroups
        //     |> List.fold (fun (acc, min) g -> 
        //         let x = possibleContiguous2 record.Record g min
        //         (x :: acc, List.min x)
        //     ) ([], -1)
        //     |> fst
            
        let rec loop toProcess processed i =
            match toProcess with
            | [] -> processed
            | x :: toProcess2 ->
                let processed2 = 
                    processed
                    |> List.collect (fun processed -> 
                        let head = List.head processed
                        let p = possiblePairs head x (record.DamagedGroups.[i])
                        p |> List.map (fun p -> p :: processed)

                    )
                loop toProcess2 processed2 (i + 1)

        let combinations = 
            match potentialCombinations with
            | [] -> []
            | fst :: tail ->
                let possibleCombinations = fst |> List.map List.singleton
                loop tail possibleCombinations 0

        combinations
        |> List.filter (fun combo -> 
            // It's possible that the way we've constructed the combinations
            // That unaccounted tailing damaged characters are not a part of a group
            let s = renderCombination record combo
            isValid record s
        )

        
    let unfold multiple (record: ConditionRecord) =
        {
            Record = 
                let sb = StringBuilder()
                for i in 1 .. multiple do
                    if i = multiple then 
                        sb.Append(record.Record) |> ignore
                    else
                        sb.Append(record.Record).Append(Unknown) |> ignore
                sb.ToString()

            DamagedGroups = 
                [
                    for i in 1 .. multiple do
                        yield! record.DamagedGroups
                ]
        }



let x = ConditionRecord.parseAll sample 

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let records = ConditionRecord.parseAll input

let sample2 = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""

let x2 = ConditionRecord.parseAll sample2

let x20 = x2[0]

let sumAllCombinations (records: ConditionRecord[]) =
    records
    |> Array.map (fun x -> x, ConditionRecord.allPossibleContiguous x)
    |> Array.map (fun (x, y) -> x, y, y.Length)
    |> Array.sumBy (fun (x,y, i) -> int64 i)

sumAllCombinations records

// x2
// |> Array.map ConditionRecord.unfold
// |> sumAllCombinations

"?###???????? 3,2,1" |> ConditionRecord.parse |> ConditionRecord.unfold 1 |> ConditionRecord.allPossibleContiguous |> List.length
"?###???????? 3,2,1" |> ConditionRecord.parse |> ConditionRecord.unfold 2 |> ConditionRecord.allPossibleContiguous |> List.length
"?###???????? 3,2,1" |> ConditionRecord.parse |> ConditionRecord.unfold 3 |> ConditionRecord.allPossibleContiguous |> List.length
"?###???????? 3,2,1" |> ConditionRecord.parse |> ConditionRecord.unfold 4 |> ConditionRecord.allPossibleContiguous |> List.length
"?###???????? 3,2,1" |> ConditionRecord.parse |> ConditionRecord.unfold 5 |> ConditionRecord.allPossibleContiguous |> List.length

let unfoldedCount id count (record: ConditionRecord) =
    let one =
        record |> ConditionRecord.allPossibleContiguous |> List.length
    let two = 
        record |> ConditionRecord.unfold 2 |> ConditionRecord.allPossibleContiguous |> List.length
    let three = 
        record |> ConditionRecord.unfold 3 |> ConditionRecord.allPossibleContiguous |> List.length
    let factor = two / one
    let factor2 = three / two

    if factor <> factor2 then
        printfn "Factor mismatch: %i %i id:%i\n%A" factor factor2 id record
        let four = 
            record |> ConditionRecord.unfold 4 |> ConditionRecord.allPossibleContiguous |> List.length

        let factor3 = four / three
        printfn "First 3 factors: %i %i %i id:%i\n%A" factor factor2 factor3 id record
        if count = 5 then 
        
            [|
                one
                factor
                factor2
                factor3
                factor3
            |] |> Array.map (bigint) |> Array.reduce ( * )


        // let five =
        //     record |> ConditionRecord.unfold 5 |> ConditionRecord.allPossibleContiguous |> List.length
        // if count = 5 then 
        //     record 
        //     |> ConditionRecord.unfold 5 
        //     |> ConditionRecord.allPossibleContiguous 
        //     |> List.length
        //     |> bigint
            // [|
            //     one
            //     two
            //     three
            //     four
            //     five
            // |] |> Array.map (bigint) |> Array.reduce ( * )
        else 
            failwithf "Unexpected count %i" count
    else
        let i = (Array.create (count - 1) (bigint (two / one))) |> Array.fold ( * ) (bigint(one)) 
        printfn "Standard: %O id:%i\n%A" i id record
        i

"?###???????? 3,2,1" |> ConditionRecord.parse |> unfoldedCount 0 5
// let unfolded = 
//     records
//     |> Array.map ConditionRecord.unfold

// unfolded 
records
|> Array.mapi (fun i x -> unfoldedCount i 5 x)
// |> Array.Parallel.map (fun x -> ConditionRecord.allPossibleContiguous x |> List.length |> int64)
|> Array.sum

// Look back at the examples. See what if there is a pattern in the number of contiguous groups
// Also look at possible filters for the combinations working from outside to in

// let path = Path.Combine(__SOURCE_DIRECTORY__, "output.txt")
// File.WriteAllText(path, "")
// records
// |> Array.map (fun x -> x, ConditionRecord.allPossibleContiguous x)
// |> Array.map (fun (x, y) -> x, y, y.Length)
// |> Array.collect (fun (x,y, i) -> 
//     [|
//         sprintf "%10s %s:: %03i" (x.Record.PadRight 20) ((String.Join(',', x.DamagedGroups)).PadRight 10) i
//         yield! y |> List.map (fun ys ->  
//             try
//                 let arr = Array.init x.Record.Length (fun _ -> Operational)
//                 let mutable i = 0
//                 let ys = List.rev ys
//                 for y in ys do
//                     let dmg = x.DamagedGroups.[i]
//                     for j in y .. (y + dmg - 1) do
//                         arr.[j] <- Damaged
//                     i <- i + 1
//                 String(arr)
//             with e ->
//                 sprintf "ERROR: %A %s" y (e.Message)
//             )
//     |]
//          )
// |> (fun s -> File.AppendAllLines(path, s))

