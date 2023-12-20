#r "nuget: FParsec, 1.1.1"

open System
open System.Collections.Generic
open System.Collections.Frozen

type Pulse =
    | High
    | Low

let [<Literal>] FlipFlopPrefix = '%'
let [<Literal>] ConjunctionPrefix = '&'

type ModuleName = string

[<Struct>]
type PulseEv = 
    {
        Origin: ModuleName
        Destination: ModuleName
        Pulse: Pulse
    }

type IPulseSink =
    abstract Receive: PulseEv -> unit

type IModule =
    abstract Name: ModuleName
    abstract Receive: origin:ModuleName * pulse:Pulse -> unit
    // abstract Tick: unit -> unit
    abstract AddSink: IPulseSink -> unit

type FlipFlop(name: ModuleName, outputs: ModuleName[]) =

    // Flip-flop modules (prefix %) are either on or off; 
    // they are initially off.  

    let ev = new Event<Pulse>()
    let mutable state = false

    override _.Equals(other) =
        match other with
        | :? IModule as b -> b.Name = name
        | _ -> false

    override _.GetHashCode() = name.GetHashCode()
    member _.Name = name
    member this.State = state

    interface IModule with
        member _.Name = name
        member this.Receive(_, pulse) =
            // If a flip-flop module receives a high pulse, it is ignored and nothing happens. 
            // However, if a flip-flop module receives a low pulse, it flips between on and off.
            // If it was off, it turns on and sends a high pulse. If it was on, it turns off and sends a low pulse.
            match pulse with
            | High -> ()
            | Low -> 
                state <- not state
                ev.Trigger(if state then High else Low)
        member this.AddSink(sink:IPulseSink) = 
            ev.Publish.Add(fun pulse -> 
                for n in outputs do
                    sink.Receive({ Origin = name; Destination = n; Pulse = pulse }))


type Conjunction(name: ModuleName, inputs: ModuleName[], outputs: ModuleName[]) =

    // Conjunction modules (prefix &) remember the type of the most recent pulse received 
    // from each of their connected input modules; they initially default to remembering a low pulse for each input. 
    // When a pulse is received, the conjunction module first updates its memory for that input. 
    // Then, if it remembers high pulses for all inputs, it sends a low pulse; otherwise, it sends a high pulse.
    let states = new Dictionary<ModuleName, Pulse>(
        seq {
            for n in inputs do
                yield KeyValuePair(n, Low)
        }
    )

    let ev = new Event<Pulse>()

    override _.Equals(other) =
        match other with
        | :? IModule as b -> b.Name = name
        | _ -> false

    override _.GetHashCode() = name.GetHashCode()
    member _.Name = name
    interface IModule with
        member _.Name = name
        member this.Receive(input, pulse) =
            states.[input] <- pulse
            let allHigh = states.Values |> Seq.forall ((=) High)
            ev.Trigger(if allHigh then Low else High)

        member this.AddSink(sink:IPulseSink) =
            ev.Publish.Add(fun pulse -> 
                for n in outputs do
                    sink.Receive({ Origin = name; Destination = n; Pulse = pulse }))

type Broadcaster(outputs: ModuleName[]) =
    let name = Broadcaster.Name
    let ev = new Event<Pulse>()

    override _.Equals(other) =
        match other with
        | :? IModule as b -> b.Name = name
        | _ -> false

    override _.GetHashCode() = name.GetHashCode()
    static member Name = "broadcaster"

    interface IModule with
        member _.Name = name
        member this.Receive(_, pulse) =
            ev.Trigger(pulse)
        member this.AddSink(sink:IPulseSink) =
            ev.Publish.Add(fun pulse -> 
                for n in outputs do
                    sink.Receive({ Origin = name; Destination = n; Pulse = pulse }))

type Button(sink: IPulseSink) =
    static member Name = "button"
    member this.Push() = sink.Receive({ Origin = Button.Name; Destination = Broadcaster.Name; Pulse = Low })


let primeFactors i =
    let rec loop i n =
        if i = 1 then
            []
        else
            if i % n = 0 then
                n :: loop (i / n) n
            else
                loop i (n + 1)
    loop i 2



type Processor(modules: IReadOnlyDictionary<ModuleName, IModule>, maxTicks) as this =
    do
        for m in modules.Values do
            m.AddSink <| (this :> IPulseSink)

    let modules = modules |> (fun x -> FrozenDictionary.ToFrozenDictionary(x, EqualityComparer<string>.Default))
    let button = Button(this)
    let pulses = new Queue<PulseEv>()

    let mutable tick = 0
    let mutable highs = 0
    let mutable lows = 0

    let mutable ng = 0
    let mutable ft = 0
    let mutable sv = 0
    let mutable jz = 0

    member _.Ticks = tick
    member _.Highs = highs
    member _.Lows = lows
    member this.PushButton() =
        tick <- tick + 1

        if tick % 1_000_000 = 0 then
            printfn "tick: %i" tick

        button.Push()
        
        while pulses.Count > 0 do
            let ev = pulses.Dequeue()
            match ev.Pulse with
            | High -> highs <- highs + 1
            | Low -> lows <- lows + 1

            match ev.Destination with
            | "xm" ->
                match ev.Pulse with
                | High -> 
                    match ev.Origin with
                    | "ng" -> ng <- tick
                    | "ft" -> ft <- tick
                    | "sv" -> sv <- tick
                    | "jz" -> jz <- tick
                    | _ -> ()
                | Low -> ()
            | _ -> ()

            match modules.TryGetValue ev.Destination with
            | false, _ -> 
                match ev.Destination with
                | "rx" -> ()
                | _ ->
                    printfn "Unknown module: %s, ticks:%i" ev.Destination tick
            | true, m -> m.Receive(ev.Origin, ev.Pulse)


    member this.Run() =
        while tick < maxTicks do
            this.PushButton()

    member this.RunPart2() =

        while (ng * ft * sv * jz) = 0 do
            this.PushButton()
        
        [
            ng
            ft
            sv
            jz
        ] |> List.collect primeFactors
        |> List.map int64
        |> Set
        |> Seq.reduce ( * )

    interface IPulseSink with
        member _.Receive ev =
            pulses.Enqueue ev

[<RequireQualifiedAccess>]
type ModuleType =
    | FlipFlop
    | Conjunction
    | Broadcaster

type ModuleDef =
    {
        Name: ModuleName
        Type: ModuleType
        Output: ModuleName list
    }

type Input =
    {
        Modules: ModuleDef list
    }

module Input =
    open FParsec
    let sample = """broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"""
    let pArrow = pstring " -> "

    let pBroadcaster = pstring "broadcaster"
    let pFlipFlop = pchar FlipFlopPrefix .>>. (many1Satisfy isLetter |>> string)
    let pConjunction = pchar ConjunctionPrefix .>>. (many1Satisfy isLetter |>> string)

    let pModuleDef: Parser<_,unit> = 
        let pOutput = 
            pArrow >>. sepBy1 (many1Satisfy isLetter |>> string) (pstring ", ")

        let pBroadcaster = pBroadcaster >>. pOutput |>> fun names -> { Name = "broadcaster"; Type = ModuleType.Broadcaster; Output = names }
        let pFlipFlop = pFlipFlop .>>. pOutput |>> fun ((_, name), names) -> { Name = name; Type = ModuleType.FlipFlop; Output = names }
        let pConjunction = pConjunction .>>. pOutput |>> fun ((_, name), names) -> { Name = name; Type = ModuleType.Conjunction; Output = names }

        [ pBroadcaster; pFlipFlop; pConjunction ]
        |> List.map attempt
        |> choice

    let pInput = many1Till (pModuleDef .>> newline) eof |>> fun modules -> { Modules = modules }

    let tryParse (s: string) =
        match run pInput s with
        | Success(result, _, _) -> Some result
        | Failure(_, _, _) -> None

    let toProcessor (input: Input) maxTicks =
        let mutable conjunctions = 
            input.Modules
            |> List.filter (fun m -> m.Type = ModuleType.Conjunction)
            |> List.map (fun m -> m.Name, [])
            |> Map.ofList

        do
            input.Modules
            |> List.iter (fun m -> 
                m.Output
                |> List.iter (fun o ->
                    match conjunctions.TryFind o with
                    | Some l -> conjunctions <- conjunctions |> Map.add o (m.Name :: l)
                    | None -> ()
                )
            )

        let modules = 
            input.Modules
            |> List.map (fun m ->
                match m.Type with
                | ModuleType.Broadcaster -> m.Name, Broadcaster(Array.ofList m.Output) :> IModule
                | ModuleType.FlipFlop -> m.Name, FlipFlop(m.Name, Array.ofList m.Output) :> _
                | ModuleType.Conjunction -> m.Name, Conjunction(m.Name,Array.ofList (conjunctions[m.Name]) ,  Array.ofList m.Output) :> _
            )
            |> List.map KeyValuePair
            |> Dictionary

        Processor(modules, maxTicks)

let parsedSample = (Input.tryParse Input.sample).Value

let sampleProcessor = Input.toProcessor parsedSample 1000

sampleProcessor.PushButton()
sampleProcessor.Highs
sampleProcessor.Lows
sampleProcessor.Run()

let sample2 = """broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
"""

let parsedSample2 = (Input.tryParse sample2).Value
let sampleProcessor2 = Input.toProcessor parsedSample2 1000
sampleProcessor2.Run()
sampleProcessor2.Highs
sampleProcessor2.Lows

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let parsedInput = (Input.tryParse input).Value

let processorPart1 = Input.toProcessor parsedInput 1_000
processorPart1.Run()
let part1Answer = processorPart1.Highs * processorPart1.Lows
let processorPart2 = Input.toProcessor parsedInput 5_000
let part2Answer = processorPart2.RunPart2()
