#r "nuget: FParsec, 1.1.1"

let sample =
    """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

type Input = { Rows: int64 list list }

module Row =
    let getDifferences (row: int64 list) =
        let rec loop allZero x1 (tail: int64 list) (diffs: int64 list) =
            match tail with
            | [] -> (allZero, List.rev diffs)
            | x2 :: xs ->
                let diff = x1 - x2
                let allZero = allZero && diff = 0L
                loop allZero x2 xs (diff :: diffs)

        match row with
        | [] -> failwith "Empty row"
        | head :: tail -> loop true head tail []

    let getRecDifferences (row: int64 list) =
        let rec loop head tail =
            match getDifferences head with
            | (true, _) -> head :: tail
            | (_, diffs) -> loop diffs (head :: tail)

        loop (List.rev row) []

    let getNext (rows: int64 list list) =
        let rec loop (rows: int64 list list) (prev: int64) =
            match rows with
            | [] -> []
            | row :: rows ->
                match row with
                | [] -> failwith "Empty row"
                | x :: xs ->
                    let next = prev + x
                    (next :: x :: xs) :: loop rows next

        loop rows 0L

module Input =
    open FParsec

    let pRow = sepBy1 pint64 (pchar ' ')

    let parse (s: string) =
        match run (many (pRow .>> ((newline >>% ()) <|> eof))) s with
        | Success (result, _, _) -> Some { Rows = result }
        | Failure (msg, _, _) ->
            printfn "Error: %s" msg
            None

    let getLastHead xs = xs |> List.last |> List.head

    let sumOfNexts (x: Input) =
        x.Rows
        |> List.map (
            Row.getRecDifferences
            >> Row.getNext
            >> getLastHead
        )
        |> List.sum


let parsedSample = Input.parse sample |> Option.get

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let parsedInput = input |> Input.parse |> Option.get

let sumPart1 = Input.sumOfNexts parsedInput

let sumPart2 =
    Input.sumOfNexts (
        {
            Rows = parsedInput.Rows |> List.map List.rev
        }
    )
