open System

let [<Literal>] Ash = '.'
let [<Literal>] Rocks = '#'

let patternSummary (colsToLeft) rowsAbove = (rowsAbove * 100) + colsToLeft

type InputGrid =
    {
        // All strings are the same length
        Grid: string[]
    }
    member this.Width = this.Grid.[0].Length
    member this.Height = this.Grid.Length
    member this.Item with get (x, y) = this.Grid.[y].[x]

let sample = """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#."""

let sample2 = """#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""

module InputGrid =

    let sequenceEqual (left: ReadOnlySpan<char>) (right: ReadOnlySpan<char>) =
        if left.Length <> right.Length then
            failwith "Spans must be the same width to be compared"
        else
            left.SequenceEqual(right)

    let unequalCount (left: ReadOnlySpan<char>) (right: ReadOnlySpan<char>) =
        if left.Length <> right.Length then
            failwith "Spans must be the same width to be compared"
        else
            let mutable count = 0
            for i = 0 to left.Length - 1 do
                if left.[i] <> right.[i] then
                    count <- count + 1
            count

    [<Obsolete>]
    let copyToReverse (s: string) (arr: char[]) =
        for i = 0 to s.Length - 1 do
            arr.[i] <- s.[s.Length - i - 1]

    let verticalCols colsToLeft width =
        let mutable left = colsToLeft  - 1
        let mutable right = colsToLeft 
        [|
            while left >= 0 && right < width do
                yield left, right
                left <- left - 1
                right <- right + 1
        |]

    let horizontalRows rowsAbove height =
        let mutable top = rowsAbove - 1
        let mutable bottom = rowsAbove
        [|
            while top >= 0 && bottom < height do
                yield top, bottom
                top <- top - 1
                bottom <- bottom + 1
        |]

    let copyVerticalSlice (grid: InputGrid) (left, right) (leftScratch: char[]) (rightScratch: char[]) =
        for y = 0 to grid.Height - 1 do
            leftScratch.[y] <- grid.[left, y]
            rightScratch.[y] <- grid.[right, y]
        
    let tryFindVerticallyReflectedColumnsToLeft (grid: InputGrid) = 
        let leftScratch = Array.zeroCreate<char> grid.Height
        let rightScratch = Array.zeroCreate<char> grid.Height
        let mutable colsToLeft = 1
        let mutable foundCols = None
        while foundCols.IsNone && colsToLeft < grid.Width do
            let arr = verticalCols colsToLeft grid.Width
            let found =
                Array.TrueForAll(arr, (fun lr ->
                    copyVerticalSlice grid lr leftScratch rightScratch
                    sequenceEqual (ReadOnlySpan(leftScratch)) (ReadOnlySpan(rightScratch))
                ))
            if found then
                foundCols <- Some colsToLeft

            colsToLeft <- colsToLeft + 1


        foundCols

    let tryFindHorizontallyReflectedRowsAbove (grid: InputGrid) = 
        let mutable rowsAbove = 1
        let mutable foundRows = None
        while foundRows.IsNone && rowsAbove < grid.Height do
            let arr = horizontalRows rowsAbove grid.Height
            let found =
                Array.TrueForAll(arr, (fun (above, below) ->
                    sequenceEqual (grid.Grid.[above].AsSpan()) (grid.Grid.[below].AsSpan())
                ))
            if found then
                foundRows <- Some rowsAbove

            rowsAbove <- rowsAbove + 1

        foundRows
        
    let tryFindVerticallyReflectedColumnsToLeftPart2 (grid: InputGrid) = 
        let leftScratch = Array.zeroCreate<char> grid.Height
        let rightScratch = Array.zeroCreate<char> grid.Height
        let mutable colsToLeft = 1
        let mutable foundCols = None
        while foundCols.IsNone && colsToLeft < grid.Width do
            let arr = verticalCols colsToLeft grid.Width
            let unequalCount =
                arr |> Array.sumBy (fun lr ->
                    copyVerticalSlice grid lr leftScratch rightScratch
                    unequalCount (ReadOnlySpan(leftScratch)) (ReadOnlySpan(rightScratch))
                )
            if unequalCount = 1 then
                foundCols <- Some colsToLeft

            colsToLeft <- colsToLeft + 1


        foundCols

    let tryFindHorizontallyReflectedRowsAbovePart2 (grid: InputGrid) = 
        let mutable rowsAbove = 1
        let mutable foundRows = None
        while foundRows.IsNone && rowsAbove < grid.Height do
            let arr = horizontalRows rowsAbove grid.Height
            let unequalCount =
                arr |> Array.sumBy (fun (above, below) ->
                    unequalCount (grid.Grid.[above].AsSpan()) (grid.Grid.[below].AsSpan())
                )
            if unequalCount = 1 then
                foundRows <- Some rowsAbove

            rowsAbove <- rowsAbove + 1

        foundRows


    let parse (input: string) =
        { Grid = input.Split('\n', StringSplitOptions.RemoveEmptyEntries) }

    let parseAll (input: string) =
        let grids = input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
        grids |> Array.map parse


let sampleGrid = InputGrid.parse sample
let sampleGrid2 = InputGrid.parse sample2
sampleGrid |> InputGrid.tryFindVerticallyReflectedColumnsToLeft
sampleGrid2 |> InputGrid.tryFindVerticallyReflectedColumnsToLeft
sampleGrid |> InputGrid.tryFindHorizontallyReflectedRowsAbove
sampleGrid2 |> InputGrid.tryFindHorizontallyReflectedRowsAbove

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let grids = InputGrid.parseAll input 

let sumReflectionLines (grid: InputGrid[]) =
    grid
    |> Array.mapi (fun i grid -> 
        try
            grid 
            |> InputGrid.tryFindVerticallyReflectedColumnsToLeft
            |> function
            | Some colsToLeft -> colsToLeft
            | None -> 
                grid 
                |> InputGrid.tryFindHorizontallyReflectedRowsAbove
                |> function
                | Some rowsAbove -> rowsAbove * 100
                | None -> 0
        with ex ->
            printfn "Failed on grid %d\n%A" i (grid)
            0
        )
    |> Array.sum

sumReflectionLines grids
let sumReflectionLinesPart2 (grid: InputGrid[]) =
    grid
    |> Array.mapi (fun i grid -> 
        try
            grid 
            |> InputGrid.tryFindVerticallyReflectedColumnsToLeftPart2
            |> function
            | Some colsToLeft -> colsToLeft
            | None -> 
                grid 
                |> InputGrid.tryFindHorizontallyReflectedRowsAbovePart2
                |> function
                | Some rowsAbove -> rowsAbove * 100
                | None -> 0
        with ex ->
            printfn "Failed on grid %d\n%A" i (grid)
            0
        )
    |> Array.sum

sumReflectionLinesPart2 grids
