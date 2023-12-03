open System
open System.Collections.Immutable
open System.IO

let input = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

[<RequireQualifiedAccess>]
type CubeBag = { Red: int; Green: int; Blue: int }

[<RequireQualifiedAccess>]
type GameRound = { Red: int; Green: int; Blue: int }

type Game =
    { Id: int
      Rounds: ImmutableArray<GameRound> }

module Game =
    let getNumber (s: ReadOnlySpan<char>) (i: int) =
        let mutable i = i
        let mutable factor = 1
        let mutable result = 0

        while i >= 0 && s.[i] <> ' ' do
            result <- result + (int (s.[i]) - 48) * factor
            factor <- factor * 10
            i <- i - 1

        result

    let getGameId (s: ReadOnlySpan<char>) =
        match s.IndexOf(':') with
        | -1 -> failwith "Invalid game line did not contain :"
        | i -> i, getNumber s (i - 1)

    let parseRound (s: ReadOnlySpan<char>) =
        // 4 green, 2 blue, 10 red
        let green =
            match s.IndexOf("green") with
            | -1 -> 0
            | i -> getNumber s (i - 2)

        let blue =
            match s.IndexOf("blue") with
            | -1 -> 0
            | i -> getNumber s (i - 2)

        let red =
            match s.IndexOf("red") with
            | -1 -> 0
            | i -> getNumber s (i - 2)

        { GameRound.Red = red
          GameRound.Green = green
          GameRound.Blue = blue }

    let parse (s: string) : Game =
        let s = s.AsSpan()
        let startI, id = getGameId s
        let mutable s = s.Slice(startI + 2)
        let rounds = ImmutableArray.CreateBuilder<GameRound>()
        let mutable i = s.IndexOf(';')

        while i > 0 do
            let round = parseRound (s.Slice(0, i))
            rounds.Add(round)
            s <- s.Slice(i + 2)
            i <- s.IndexOf(';')

        let round = parseRound s
        rounds.Add(round)

        { Id = id
          Rounds = rounds.ToImmutable() }


    let wasPossible (bag: CubeBag) (game: Game) =
        let mutable possible = true

        for round in game.Rounds do
            possible <-
                possible
                && round.Red <= bag.Red
                && round.Green <= bag.Green
                && round.Blue <= bag.Blue

        possible

    let minimumPossible (game: Game) =
        let mutable minRed = 0
        let mutable minGreen = 0
        let mutable minBlue = 0

        for round in game.Rounds do
            minRed <- max minRed round.Red
            minGreen <- max minGreen round.Green
            minBlue <- max minBlue round.Blue

        { CubeBag.Red = minRed
          CubeBag.Green = minGreen
          CubeBag.Blue = minBlue }

module CubeBag =
    let power (bag: CubeBag) = bag.Red * bag.Green * bag.Blue

let inputSample =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""

let gamesSample =
    inputSample.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map Game.parse

let possibleIdsSumSample =
    let bag =
        { CubeBag.Red = 12
          CubeBag.Green = 13
          CubeBag.Blue = 14 }

    gamesSample
    |> Array.filter (Game.wasPossible bag)
    |> Array.map _.Id
    |> Array.sum

let games = input |> Array.map Game.parse

let possibleIdsSum =
    let bag =
        { CubeBag.Red = 12
          CubeBag.Green = 13
          CubeBag.Blue = 14 }

    games
    |> Array.filter (Game.wasPossible bag)
    |> Array.map _.Id
    |> Array.sum

let powerSum =
    games
    |> Array.map Game.minimumPossible
    |> Array.map CubeBag.power
    |> Array.sum
