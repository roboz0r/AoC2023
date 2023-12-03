open System
open System.IO
open System.Collections.Immutable

let sample =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

module ImmutableArray =

    let map (f: 'T -> 'U) (source: ImmutableArray<'T>) =
        let builder = ImmutableArray.CreateBuilder<'U>(source.Length)

        for item in source do
            builder.Add(f item)

        builder.MoveToImmutable()

    let choose (f: 'T -> 'U option) (source: ImmutableArray<'T>) =
        let builder = ImmutableArray.CreateBuilder<'U>(source.Length)

        for item in source do
            match f item with
            | Some x -> builder.Add(x)
            | None -> ()

        builder.ToImmutable()

    let collect (f: 'T -> ImmutableArray<'U>) (source: ImmutableArray<'T>) =
        let builder = ImmutableArray.CreateBuilder<'U>(source.Length)

        for item in source do
            builder.AddRange(f item)

        builder.ToImmutable()

    let sum (source: ImmutableArray<int>) =
        let mutable sum = 0

        for item in source do
            sum <- sum + item

        sum

    let sumBy (f: 'T -> int) (source: ImmutableArray<'T>) =
        let mutable sum = 0

        for item in source do
            sum <- sum + f item

        sum



let sampleLines =
    sample.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
    |> ImmutableArray.CreateRange

let isDigit x = x >= '0' && x <= '9'

let (|Digit|Not|) =
    function
    | x when isDigit x -> Digit
    | _ -> Not

type PartNo =
    { Line: string
      Start: int
      Length: int }
    member this.Value = Int32.Parse(this.Line.AsSpan().Slice(this.Start, this.Length))

module PartNo =
    let fromIndex (line: string) i =
        if not (isDigit (line[i])) then
            failwith $"Unexpected char {line[i]} in {line} at {i} when creating PartNo"

        let mutable start = i

        while start > 0 && isDigit line[start - 1] do
            start <- start - 1

        let mutable endC = i

        while endC < (line.Length - 1) && isDigit line[endC + 1] do
            endC <- endC + 1

        { Line = line
          Start = start
          Length = endC - start + 1 }


type Symbol =
    { Lines: ImmutableArray<string>
      LineId: int
      Start: int }

    member this.Char = this.Lines.[this.LineId].[this.Start]

module Symbol =

    let getAdjacentParts (symbol: Symbol) =
        let partFragments = ImmutableArray.CreateBuilder<PartNo>()

        let line = symbol.Lines[symbol.LineId - 1]

        match line[symbol.Start - 1], line[symbol.Start], line[symbol.Start + 1] with
        | Digit, Digit, Digit
        | Digit, Digit, Not
        | Digit, Not, Not -> partFragments.Add(PartNo.fromIndex line (symbol.Start - 1))
        | Digit, Not, Digit ->
            partFragments.Add(PartNo.fromIndex line (symbol.Start - 1))
            partFragments.Add(PartNo.fromIndex line (symbol.Start + 1))
        | Not, Digit, Digit
        | Not, Digit, Not -> partFragments.Add(PartNo.fromIndex line symbol.Start)
        | Not, Not, Digit -> partFragments.Add(PartNo.fromIndex line (symbol.Start + 1))
        | Not, Not, Not -> ()

        let line = symbol.Lines[symbol.LineId]

        match line[symbol.Start - 1], line[symbol.Start + 1] with
        | Digit, Digit ->
            partFragments.Add(PartNo.fromIndex line (symbol.Start - 1))
            partFragments.Add(PartNo.fromIndex line (symbol.Start + 1))
        | Digit, Not -> 
            partFragments.Add(PartNo.fromIndex line (symbol.Start - 1))
        | Not, Digit ->
            partFragments.Add(PartNo.fromIndex line (symbol.Start + 1))
        | Not, Not -> ()

        let line = symbol.Lines[symbol.LineId + 1]

        match line[symbol.Start - 1], line[symbol.Start], line[symbol.Start + 1] with
        | Digit, Digit, Digit
        | Digit, Digit, Not
        | Digit, Not, Not -> partFragments.Add(PartNo.fromIndex line (symbol.Start - 1))
        | Digit, Not, Digit ->
            partFragments.Add(PartNo.fromIndex line (symbol.Start - 1))
            partFragments.Add(PartNo.fromIndex line (symbol.Start + 1))
        | Not, Digit, Digit
        | Not, Digit, Not -> partFragments.Add(PartNo.fromIndex line symbol.Start)
        | Not, Not, Digit -> partFragments.Add(PartNo.fromIndex line (symbol.Start + 1))
        | Not, Not, Not -> ()

        partFragments.ToImmutable()



let isPeriodOrDigit (c: char) =
    match c with
    | '.' -> true
    | c when c >= '0' && c <= '9' -> true
    | _ -> false

let tryGetSymbol (s: ReadOnlySpan<char>) =
    let mutable i = 0

    while i < s.Length && (isPeriodOrDigit s.[i]) do
        i <- i + 1

    if i < s.Length then Some i else None

// There are numbers but no symbols in the first or last lines or first and last columns
let getAllSymbols (lines: ImmutableArray<string>) =
    let symbols = ImmutableArray.CreateBuilder<Symbol>()

    for lineId in 1 .. lines.Length - 2 do
        let line = lines.[lineId]
        let mutable lineS = line.AsSpan()
        let mutable start = 0
        let mutable _continue = true

        while _continue do
            match tryGetSymbol lineS with
            | Some i ->
                symbols.Add
                    { Lines = lines |> ImmutableArray.CreateRange
                      LineId = lineId
                      Start = start + i }

                lineS <- lineS.Slice(i + 1)
                start <- start + i + 1
            | None -> _continue <- false

    symbols.ToImmutable()

let getResultPart1 (lines: ImmutableArray<string>) =
    lines
    |> getAllSymbols
    |> ImmutableArray.collect Symbol.getAdjacentParts
    |> ImmutableArray.sumBy (_.Value)

let input = 
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))
    |> ImmutableArray.CreateRange

let result = getResultPart1 input

type Gear = 
    {
        Symbol: Symbol
        Part1: PartNo
        Part2: PartNo
    }
    member this.Ratio = this.Part1.Value * this.Part2.Value

module Gear =
    let tryGetGear (symbol: Symbol) =
        let parts = Symbol.getAdjacentParts symbol
        if parts.Length <> 2 then None
        else
            Some { Symbol = symbol
                   Part1 = parts.[0]
                   Part2 = parts.[1] }

let getResultPart2 (lines: ImmutableArray<string>) =
    lines
    |> getAllSymbols
    |> ImmutableArray.choose Gear.tryGetGear
    |> ImmutableArray.sumBy _.Ratio

let result2 = getResultPart2 input
