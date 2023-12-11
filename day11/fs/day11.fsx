open System
open System.Text

let sample = 
    """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""

let [<Literal>] Empty = '.'
let [<Literal>] Galaxy = '#'

type GalaxyPosition = { X: int; Y: int }


type Image = 
    {
        Image: string[]
        Width: int
        Height: int
    }
    member this.Item with get (x, y) = this.Image.[y][x]

type ExpandedImage =
    {
        Image: Image
        ExpansionFactor: int
        ExpandedRows: int[]
        ExpandedCols: int[]
    }
    

module GalaxyPosition = 
    let distanceBetween (a: GalaxyPosition) (b: GalaxyPosition) = 
        let x = abs (a.X - b.X)
        let y = abs (a.Y - b.Y)
        x + y

    distanceBetween { X = 1; Y = 6 } { X = 5; Y = 11 }

    let createPairs (positions: GalaxyPosition[]) = 
        [|
            for i in 0 .. positions.Length - 2 do
                for j in i + 1 .. positions.Length - 1 do
                    positions.[i], positions.[j]
        |]
module Image =
    let parse (input: string) =
        let lines = input.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
        let width = lines.[0].Length
        let height = lines.Length
        {
            Image = lines
            Width = width
            Height = height
        }
    let listGalaxies (image: Image) = 
        image.Image
         |> Array.mapi (fun y c -> 
            c 
            |> Seq.mapi (fun x c -> { X = x; Y = y }, c)
            |> Seq.choose (fun (x, c) -> if c = Galaxy then Some x else None)
         )
         |> Seq.concat
         |> Array.ofSeq

    let columIsEmpty (image: Image) x =
        image.Image
        |> Array.tryFind (fun line -> line.[x] = Galaxy)
        |> _.IsNone

    let rowIsEmpty (image: Image) y =
        match image.Image.[y].IndexOf(Galaxy) with
        | -1 -> true
        | _ -> false

    let expandGalaxy image =
        let colMap = 
            [|
                for x in 0 .. image.Width - 1 do
                    Some x
                    if columIsEmpty image x then 
                        None
            |]
        let rowMap = 
            [|
                for y in 0 .. image.Height - 1 do
                    Some y
                    if rowIsEmpty image y then 
                        None
            |]

        let newImage = 
            [|
                for y in rowMap do
                    match y with
                    | Some y ->
                        let sb = new StringBuilder()
                        for x in colMap do
                            match x with
                            | Some x -> sb.Append(image.[x, y]) |> ignore
                            | _ ->  sb.Append(Empty) |> ignore
                        sb.ToString()
                    | None -> String(Empty, colMap.Length)
            |]
        {
            Image = newImage
            Width = colMap.Length
            Height = rowMap.Length
        }

    let expandGalaxy2 expansion image =
        let colMap = 
            [|
                for x in 0 .. image.Width - 1 do
                    if columIsEmpty image x then 
                        x
            |]
        let rowMap = 
            [|
                for y in 0 .. image.Height - 1 do
                    if rowIsEmpty image y then 
                        y
            |]

        {
            Image = image
            ExpansionFactor = expansion
            ExpandedRows = rowMap
            ExpandedCols = colMap
        }

module ExpandedImage = 
    let distanceBetween (image: ExpandedImage) (a: GalaxyPosition) (b: GalaxyPosition) = 
        let rowsToCheck = 
            if a.Y < b.Y then
                [| (a.Y + 1) .. (b.Y - 1) |]
            else
                [| (b.Y + 1) .. (a.Y - 1) |]
        let colsToCheck =
            if a.X < b.X then
                [| (a.X + 1) .. (b.X - 1) |]
            else
                [| (b.X + 1) .. (a.X - 1) |]
        let x = abs (a.X - b.X)
        let y = abs (a.Y - b.Y)

        let expandedRows = 
                rowsToCheck
                |> Array.choose (fun y -> 
                    image.ExpandedRows 
                    |> Array.tryFindIndex ((=) y)
                    |> Option.map (fun _ -> image.ExpansionFactor)
                )
        let expandedCols = 
                colsToCheck 
                |> Array.choose (fun x -> 
                    image.ExpandedCols 
                    |> Array.tryFindIndex ((=) x)
                    |> Option.map (fun _ -> image.ExpansionFactor)
                )

        [|
            x
            y
            yield! expandedRows
            yield! expandedCols
            if image.ExpansionFactor > 1 then 
                -expandedRows.Length
                -expandedCols.Length

        |] 
        |> Array.map int64
        |> Array.sum


let imageSample = Image.parse sample

let sumExpandedGalaxies image =
    image
    |> Image.expandGalaxy
    |> Image.listGalaxies
    |> GalaxyPosition.createPairs
    |> Array.map (fun (a, b) -> GalaxyPosition.distanceBetween a b)
    |> Array.sum

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let image = Image.parse input
sumExpandedGalaxies image


let sumExpandedGalaxies2 expansion image =
    let expImage =
        image
        |> (Image.expandGalaxy2 expansion)

    let galaxies = 
        image
        |> Image.listGalaxies
        |> GalaxyPosition.createPairs

    galaxies
    |> Array.map (fun (a, b) -> ExpandedImage.distanceBetween expImage a b)
    |> Array.sum

sumExpandedGalaxies2 1_000_000 image