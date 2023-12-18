open System

let [<Literal>] Empty = '.'
let [<Literal>] RMirror = '/'
let [<Literal>] LMirror = '\\'
let [<Literal>] VSplitter = '|'
let [<Literal>] HSplitter = '-'
let [<Literal>] Energized = '#'

let sample = """.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."""

type Layout =
    {
        Rows: string[]
    }
    member this.Width = this.Rows.[0].Length
    member this.Height = this.Rows.Length
    member this.Item with get (x, y) = this.Rows.[y].[x]

type Direction =
    | Up
    | Down
    | Left
    | Right
    member this.Value =
        match this with
        | Up -> 0
        | Down -> 1
        | Left -> 2
        | Right -> 3
    member this.Char =
        match this with
        | Up -> '^'
        | Down -> 'v'
        | Left -> '<'
        | Right -> '>'

type Light = 
    {
        X: int
        Y: int
        Direction: Direction
    }
    static member Start =
        {
            X = 0
            Y = 0
            Direction = Right
        }

module Layout = 
    let parse (input: string) = 
        let rows = input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        { Rows = rows }

    let initEnergized (layout: Layout) = 
        Array2D.init layout.Width layout.Height (fun _ _ -> (Array.create 4 None))

    let printDirections (layout: Layout) (energized: (Light option[])[,]) = 
        let xs = 
            Array2D.init layout.Width layout.Height (fun x y -> 
                let en = 
                    energized.[x, y]
                    |> Array.choose id
                match en.Length with
                | 0 -> layout[x,y]
                | 1 -> en.[0].Direction.Char
                | x ->char (x + 48)
                
            )
        for y in 0 .. layout.Height - 1 do
            for x in 0 .. layout.Width - 1 do
                printf "%c" (xs.[x, y])
            printfn ""

    let traceLight (light: Light) (layout: Layout) = 
        let energized = initEnergized layout
        // printDirections layout energized
        let energize (light: Light) = 
            let x, y, i = light.X, light.Y, light.Direction.Value
            match energized.[x, y][i] with
            | None -> 
                energized.[x, y][i] <- Some light
                // printfn "%A" energized
                // printDirections layout energized
                true
            | Some _ -> 
                false
        let rec loop (light: Light) =
            let x, y = light.X, light.Y
            if energize light then 
                match light.Direction with
                | Up -> 
                    if y = 0 then ()
                    else
                        match layout.[x, y - 1] with
                        | Empty -> loop { light with Y = y - 1 }
                        | RMirror -> loop { light with Direction = Right; Y = y - 1 }
                        | LMirror -> loop { light with Direction = Left; Y = y - 1 }
                        | VSplitter -> loop { light with Y = y - 1 }
                        | HSplitter -> 
                            loop { light with Direction = Left; Y = y - 1 }
                            loop { light with Direction = Right; Y = y - 1 }
                        | c -> failwith $"Unexpected character at {x}, {y}: {c}"
                        
                | Down ->
                    if y = layout.Height - 1 then ()
                    else
                        match layout.[x, y + 1] with
                        | Empty -> loop { light with Y = y + 1 }
                        | RMirror -> loop { light with Direction = Left; Y = y + 1 }
                        | LMirror -> loop { light with Direction = Right; Y = y + 1 }
                        | VSplitter -> loop { light with Y = y + 1 }
                        | HSplitter -> 
                            loop { light with Direction = Left; Y = y + 1 }
                            loop { light with Direction = Right; Y = y + 1 }
                        | c -> failwith $"Unexpected character at {x}, {y}: {c}"

                | Left ->
                    if x = 0 then ()
                    else
                        match layout.[x - 1, y] with
                        | Empty -> loop { light with X = x - 1 }
                        | RMirror -> loop { light with Direction = Down; X = x - 1 }
                        | LMirror -> loop { light with Direction = Up; X = x - 1 }
                        | VSplitter -> 
                            loop { light with Direction = Up; X = x - 1 }
                            loop { light with Direction = Down; X = x - 1 }
                        | HSplitter -> loop { light with X = x - 1 }
                        | c -> failwith $"Unexpected character at {x}, {y}: {c}"

                | Right ->
                    if x = layout.Width - 1 then ()
                    else
                        match layout.[x + 1, y] with
                        | Empty -> loop { light with X = x + 1 }
                        | RMirror -> loop { light with Direction = Up; X = x + 1 }
                        | LMirror -> loop { light with Direction = Down; X = x + 1 }
                        | VSplitter -> 
                            loop { light with Direction = Up; X = x + 1 }
                            loop { light with Direction = Down; X = x + 1 }
                        | HSplitter -> loop { light with X = x + 1 }
                        | c -> failwith $"Unexpected character at {x}, {y}: {c}"

        loop light
        let mutable energizedCount = 0
        // printDirections layout energized
        energized |> Array2D.iter (fun arr -> if (arr |> Array.tryFind Option.isSome).IsSome then energizedCount <- energizedCount + 1)
        energizedCount

let sampleLayout =
    sample
    |> Layout.parse

sampleLayout
|> (Layout.traceLight Light.Start)

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__,"..", "input.txt"))

let layout =
    input
    |> Layout.parse

layout
|> (Layout.traceLight Light.Start)

let multiStart =
    [|
        // Top
        for i in 0 .. layout.Width - 1 do
            { X = i; Y = 0; Direction = Down }
        // Bottom
        for i in 0 .. layout.Width - 1 do
            { X = i; Y = layout.Height - 1; Direction = Up }
        // Left
        for i in 0 .. layout.Height - 1 do
            { X = 0; Y = i; Direction = Right }
        // Right
        for i in 0 .. layout.Height - 1 do
            { X = layout.Width - 1; Y = i; Direction = Left }
    |]

multiStart
|> Array.map (fun light -> Layout.traceLight light layout)
|> Array.max
