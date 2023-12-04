open System

type ScratchCard =
    { Id: int
      WinningNos: int []
      ActualNos: int [] }

module ScratchCard =
    // Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    // Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    // Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    // Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    // Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    // Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

    let parse (line: string) =
        let parts = line.Split([| ':'; '|' |])
        let id = int (parts.[0].Substring(5))

        let winningNos =
            parts.[1]
                .Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int

        let actualNos =
            parts.[2]
                .Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int

        { Id = id
          WinningNos = winningNos
          ActualNos = actualNos }

    let matchingNos (x: ScratchCard) =
        x.ActualNos
        |> Array.filter (fun i -> x.WinningNos |> Array.contains i)
        |> Array.length

    let points (x: ScratchCard) =
        x
        |> matchingNos
        |> (function
        | 0 -> 0
        | x -> (1 <<< (x - 1)))

open System.IO
let input = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let cards = input |> Array.map ScratchCard.parse

let totalPointsPart1 = cards |> Array.sumBy ScratchCard.points

let dupedCardsPart2 =
    let workingCards = cards |> Array.map (fun x -> x, 1)

    workingCards
    |> Array.iteri (fun i (x, count) ->
        let matchingNos = ScratchCard.matchingNos x

        match matchingNos with
        | 0 -> ()
        | _ ->
            for j in (i + 1) .. (i + matchingNos) do
                let (y, countY) = workingCards[j]
                workingCards[j] <- y, countY + count

    )

    workingCards
    |> Array.sumBy (fun (_, count) -> count)
