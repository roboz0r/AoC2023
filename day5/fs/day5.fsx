#r "nuget: FParsec, 1.1.1"

let sample =
    """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

[<Measure>]
type Seed

[<Measure>]
type Soil

[<Measure>]
type Fertilizer

[<Measure>]
type Water

[<Measure>]
type Light

[<Measure>]
type Temperature

[<Measure>]
type Humidity

[<Measure>]
type Location

open LanguagePrimitives

type Mapping<[<Measure>] 'TSource, [<Measure>] 'TDestination> =
    { Source: int64<'TSource>
      Destination: int64<'TDestination>
      Range: int64 }
    member this.SourceRange = Int64WithMeasure<'TSource>(this.Range)
    member this.DestinationRange = Int64WithMeasure<'TDestination>(this.Range)

type Input =
    { Seeds: int64<Seed> list
      SeedToSoil: Mapping<Seed, Soil> list
      SoilToFertilizer: Mapping<Soil, Fertilizer> list
      FertilizerToWater: Mapping<Fertilizer, Water> list
      WaterToLight: Mapping<Water, Light> list
      LightToTemperature: Mapping<Light, Temperature> list
      TemperatureToHumidity: Mapping<Temperature, Humidity> list
      HumidityToLocation: Mapping<Humidity, Location> list }

[<Struct>]
type IdRange<[<Measure>] 'TSource> =
    { Id: int64<'TSource>
      Range: int64<'TSource> }

open System

module IdRange =
    let isInRange (range: IdRange<_>) (value: int64<_>) =
        value >= range.Id && value < (range.Id + range.Range)


module Mapping =

    type MapRangeResult<[<Measure>] 'TSource, [<Measure>] 'TDestination> =
        { Transformed: IdRange<'TDestination> option
          LeftOver: IdRange<'TSource> list }

    let mapImp sourceId destId range (value: int64) =
        let offset = value - sourceId

        if value >= sourceId && offset < range then
            Some(destId + offset)
        else
            None

    let inline mapRange<[<Measure>] 'TSource, [<Measure>] 'TDestination>
        (input: IdRange<'TSource>)
        (mapping: Mapping<'TSource, 'TDestination>)
        : MapRangeResult<'TSource, 'TDestination> =
        // Cases
        // 1. Input is fully in range of mapping (Some, [])
        // 2. Input range starts below mapping range and ends in range (Some, [below])
        // 3. Input range starts in range and ends above mapping range (Some, [above])
        // 4. Input range starts below mapping range and ends above mapping range (Some, [below; above])
        // 5. Input is not in mapping range (None, [input])
            if input.Id >= mapping.Source && input.Range <= mapping.SourceRange then
                { 
                    Transformed =
                        Some { 
                                Id = mapping.Destination + 1L<_> * (input.Id - mapping.Source)
                                Range = 1L<_> * input.Range 
                        }
                    LeftOver = [] 
                }
            elif input.Id < mapping.Source && (input.Id + input.Range) <= (mapping.Source + mapping.SourceRange) then
                { 
                    Transformed = 
                        Some { 
                            Id = mapping.Destination
                            Range = 1L<_> * (input.Id + input.Range - mapping.Source) 
                        }
                    LeftOver = [ { Id = input.Id; Range = mapping.Source - input.Id } ] 
                }
            elif input.Id >= mapping.Source && input.Range > mapping.SourceRange then
                {
                    Transformed =
                        Some {
                            Id = mapping.Destination + 1L<_> * (input.Id - mapping.Source)
                            Range = 1L<_> * mapping.SourceRange
                        }
                    LeftOver = [ { Id = mapping.Source + mapping.SourceRange; Range = input.Id + input.Range - (mapping.Source + mapping.SourceRange) } ]
                }
            elif input.Id < mapping.Source && (input.Id + input.Range) > (mapping.Source + mapping.SourceRange) then
                {
                    Transformed =
                        Some {
                            Id = mapping.Destination
                            Range = 1L<_> * mapping.SourceRange
                        }
                    LeftOver = [ 
                        { Id = input.Id; Range = mapping.Source - input.Id }; 
                        { Id = mapping.Source + mapping.SourceRange; Range = input.Id + input.Range - (mapping.Source + mapping.SourceRange) } 
                    ]
                }
            else
                { Transformed = None ;LeftOver = [ input ] }

    let inline mapRangeMany<[<Measure>] 'TSource, [<Measure>] 'TDestination>
        (mappings: Mapping<'TSource, 'TDestination> list)
        (value: IdRange<'TSource>)
        =
        // This is a dead end due to combinatorial explosion
        let inputRanges = ResizeArray([value])
        let outputRanges = ResizeArray()
        for mapping in mappings do
            let count = inputRanges.Count
            printfn "inputRanges.Count %d" count
            for i in (count - 1) .. -1 .. 0 do
                let inputRange = inputRanges.[i]
                let result = mapRange inputRange mapping
                match result.Transformed with
                | Some x -> outputRanges.Add(x)
                | None -> ()
                inputRanges.RemoveAt(i)
                printfn "Removed inputRanges.Count %d" inputRanges.Count
                for x in result.LeftOver do
                    inputRanges.Add(x)
            
        for input in inputRanges do
            outputRanges.Add({ Id = input.Id * 1L<_>; Range = input.Range * 1L<_> })

        outputRanges.ToArray()

    let inline map<[<Measure>] 'TSource, [<Measure>] 'TDestination>
        (mapping: Mapping<'TSource, 'TDestination>)
        (value: int64<'TSource>)
        =
        mapImp (mapping.Source * 1L<_>) (mapping.Destination * 1L<_>) mapping.Range (value * 1L<_>)
        |> Option.map Int64WithMeasure<'TDestination>

    let inline mapMany<[<Measure>] 'TSource, [<Measure>] 'TDestination>
        (mappings: Mapping<'TSource, 'TDestination> list)
        (value: int64<'TSource>)
        =
        mappings
        |> List.tryPick (fun x -> map x value)
        |> Option.defaultValue (value * 1L<_>)

module Input =
    open FParsec
    let inline int64M<[<Measure>] 'M> = Int64WithMeasure<'M>

    let pSpaceSepInt64: Parser<_, unit> =
        manyTill (pint64 .>> (opt (pchar ' '))) ((newline >>% ()) <|> eof)

    let p3Int64 createSource createDest =
        pSpaceSepInt64
        >>= function
            | [ dest; source; range ] ->
                preturn
                    { Source = createSource source
                      Destination = createDest dest
                      Range = range }
            | _ -> fail "Expected 3 ints"

    let pSeeds =
        pstring "seeds: " >>. pSpaceSepInt64 .>> newline
        |>> List.map (int64M<Seed>)

    let pSeedToSoil =
        pstring "seed-to-soil map:"
        >>. newline
        >>. manyTill (p3Int64 (int64M<Seed>) (int64M<Soil>)) newline

    let pSoilToFertilizer =
        pstring "soil-to-fertilizer map:"
        >>. newline
        >>. manyTill (p3Int64 (int64M<Soil>) (int64M<Fertilizer>)) newline

    let pFertilizerToWater =
        pstring "fertilizer-to-water map:"
        >>. newline
        >>. manyTill (p3Int64 (int64M<Fertilizer>) (int64M<Water>)) newline

    let pWaterToLight =
        pstring "water-to-light map:"
        >>. newline
        >>. manyTill (p3Int64 (int64M<Water>) (int64M<Light>)) newline

    let pLightToTemperature =
        pstring "light-to-temperature map:"
        >>. newline
        >>. manyTill (p3Int64 (int64M<Light>) (int64M<Temperature>)) newline

    let pTemperatureToHumidity =
        pstring "temperature-to-humidity map:"
        >>. newline
        >>. manyTill (p3Int64 (int64M<Temperature>) (int64M<Humidity>)) newline

    let pHumidityToLocation =
        pstring "humidity-to-location map:"
        >>. newline
        >>. many (p3Int64 (int64M<Humidity>) (int64M<Location>))

    let pInput =
        pSeeds
        >>= fun seeds ->
            pSeedToSoil
            >>= fun seedToSoil ->
                pSoilToFertilizer
                >>= fun soilToFertilizer ->
                    pFertilizerToWater
                    >>= fun fertilizerToWater ->
                        pWaterToLight
                        >>= fun waterToLight ->
                            pLightToTemperature
                            >>= fun lightToTemperature ->
                                pTemperatureToHumidity
                                >>= fun temperatureToHumidity ->
                                    pHumidityToLocation
                                    |>> fun humidityToLocation ->
                                            { 
                                                Seeds = seeds
                                                SeedToSoil = seedToSoil
                                                SoilToFertilizer = soilToFertilizer
                                                FertilizerToWater = fertilizerToWater
                                                WaterToLight = waterToLight
                                                LightToTemperature = lightToTemperature
                                                TemperatureToHumidity = temperatureToHumidity
                                                HumidityToLocation = humidityToLocation 
                                            }

    let tryParse (s: string) =
        match runParserOnString pInput () "" s with
        | Success (result, _, _) -> Some result
        | Failure (msg, _, _) ->
            printfn "Failed to parse input: %s" msg
            None

    let mapSeedToSoil (input: Input) =
        input.Seeds
        |> List.map (Mapping.mapMany input.SeedToSoil)

    let mapSeedToLocation (input: Input) =
        input.Seeds
        |> List.map (Mapping.mapMany input.SeedToSoil)
        |> List.map (Mapping.mapMany input.SoilToFertilizer)
        |> List.map (Mapping.mapMany input.FertilizerToWater)
        |> List.map (Mapping.mapMany input.WaterToLight)
        |> List.map (Mapping.mapMany input.LightToTemperature)
        |> List.map (Mapping.mapMany input.TemperatureToHumidity)
        |> List.map (Mapping.mapMany input.HumidityToLocation)

sample
|> Input.tryParse
|> Option.iter (fun input -> input |> Input.mapSeedToLocation |> printfn "%A")

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))
#time "on"

let resultPart1 =
    input
    |> Input.tryParse
    |> Option.map (fun input -> input |> Input.mapSeedToLocation |> List.min)

#time "off"

type InputPart2 =
    { SeedRanges: IdRange<Seed> list
      SoilToSeed: Mapping<Soil, Seed> list
      FertilizerToSoil: Mapping<Fertilizer, Soil> list
      WaterToFertilizer: Mapping<Water, Fertilizer> list
      LightToWater: Mapping<Light, Water> list
      TemperatureToLight: Mapping<Temperature, Light> list
      HumidityToTemperature: Mapping<Humidity, Temperature> list
      LocationToHumidity: Mapping<Location, Humidity> list }

module InputPart2 =
    let invertMapping (mapping: Mapping<_, _>) =
        { Source = mapping.Destination
          Destination = mapping.Source
          Range = mapping.Range }

    let ofInput (input: Input) =
        let rec f (xs: int64<Seed> list) =
            match xs with
            | [] -> []
            | id :: range :: xs -> { Id = id; Range = range } :: f xs
            | _ -> failwith "Expected even number of elements"

        { SeedRanges = f input.Seeds
          SoilToSeed =  input.SeedToSoil |> List.map invertMapping
          FertilizerToSoil =  input.SoilToFertilizer |> List.map invertMapping
          WaterToFertilizer =  input.FertilizerToWater |> List.map invertMapping
          LightToWater =  input.WaterToLight |> List.map invertMapping
          TemperatureToLight =  input.LightToTemperature |> List.map invertMapping
          HumidityToTemperature =  input.TemperatureToHumidity |> List.map invertMapping
          LocationToHumidity =  input.HumidityToLocation |> List.map invertMapping }

    let locationRanges (input: InputPart2) =
        input.LocationToHumidity
        |> List.map (fun mapping -> { Id = mapping.Source; Range = mapping.SourceRange })
        |> List.sortBy _.Id

    let locationToSeed (input: InputPart2) (location: int64<Location>) =
        location
        |> (Mapping.mapMany input.LocationToHumidity)
        |> (Mapping.mapMany input.HumidityToTemperature)
        |> (Mapping.mapMany input.TemperatureToLight)
        |> (Mapping.mapMany input.LightToWater)
        |> (Mapping.mapMany input.WaterToFertilizer)
        |> (Mapping.mapMany input.FertilizerToSoil)
        |> (Mapping.mapMany input.SoilToSeed)

    let locationInSeedRanges input (seedRanges: IdRange<Seed>[]) (location: int64<Location>) =
        let seed = locationToSeed input location
        let mutable notInRange = true
        let mutable i = 0

        while notInRange && i < seedRanges.Length do
            if IdRange.isInRange (seedRanges[i]) seed then
                notInRange <- false

            i <- i + 1

        not notInRange


#time "on"
let resultPart2 =
    input
    |> Input.tryParse
    |> Option.map InputPart2.ofInput
    |> Option.bind (fun input -> 
        let seedRanges = List.toArray input.SeedRanges
        let locationRanges = input |> InputPart2.locationRanges
        let mutable i = 0
        let mutable j = 0L<_>
        let mutable lowestLocation = None
        while lowestLocation.IsNone && i < locationRanges.Length do
            let locationRange = locationRanges.[i]
            j <- locationRange.Id
            while lowestLocation.IsNone && j < (locationRange.Id + locationRange.Range) do
                if InputPart2.locationInSeedRanges input seedRanges j then 
                    lowestLocation <- Some(j)
                j <- j + 1L<_>
        lowestLocation
    )
#time "off"