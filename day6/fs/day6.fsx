#r "nuget: FParsec, 1.1.1"

open System
open FParsec
open LanguagePrimitives

let sample = """Time:      7  15   30
Distance:  9  40  200"""

[<Measure>] type ms
[<Measure>] type mm

type Race =
    {
        Time: int<ms>
        Distance: int64<mm>
    }

type Input =
    {
        Times: int<ms> list
        Distances: int64<mm> list
    }

module Input =

    let pTimes = pstring "Time:" >>. (manyTill (spaces >>. pint32) newline) |>> List.map Int32WithMeasure<ms>
    let pDistances = pstring "Distance:" >>. (manyTill (spaces >>. pint64) (eof <|> (newline >>% ()))) |>> List.map Int64WithMeasure<mm>

    let pInput: Parser<_,unit> = pipe2 pTimes pDistances (fun times distances -> { Times = times; Distances = distances })

    let toRaces (input: Input) =
        List.zip input.Times input.Distances
        |> List.map (fun (time, distance) -> { Time = time; Distance = distance })

    let tryParseInput (input: string) =
        match run pInput input with
        | Success (result, _, _) -> Some result
        | Failure (msg, _, _) -> 
            printfn "Error: %s" msg
            None

module Race =
    let boatAcceleration = 1<mm/ms/ms>
    let getDistance timeHeld raceTime =
        let initialSpeed: int<mm/ms> = timeHeld * boatAcceleration
        if timeHeld >= raceTime then 
            {
                Time = raceTime
                Distance = 0L<_>
            }
        else
            {
                Time = raceTime
                Distance = (int64 initialSpeed) * (int64 ((raceTime - timeHeld) * 1<1/ms>) * 1L<mm>)
            }

    let allDistances raceTime =
        // Only need half the times, since the other half is just the reverse of the first half
        let count = raceTime / 2<ms> + ((raceTime * 1<_>) % 2)
        Array.init count (fun i -> getDistance (Int32WithMeasure<_> i) raceTime)
        
    let numDistancesFurther race = 
        let allDistances = allDistances race.Time
        allDistances
        |> Array.filter (fun x -> x.Distance > race.Distance)
        |> Array.length
        |> fun x -> 
            match ((race.Time * 1<_>) % 2) with
            | 0 -> x * 2 + 1
            | _ -> x * 2

sample
|> Input.tryParseInput
|> Option.map Input.toRaces
|> Option.map (List.map Race.numDistancesFurther)
|> Option.map (List.reduce (*))
|> Option.defaultValue 0

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))


input
|> Input.tryParseInput
|> Option.map Input.toRaces
|> Option.map (List.map Race.numDistancesFurther)
|> Option.map (List.reduce (*))
|> Option.defaultValue 0

{
    Time =      45977295<_>
    Distance =  305106211101695L<_>
}
|> Race.numDistancesFurther