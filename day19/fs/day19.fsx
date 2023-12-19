#r "nuget: FParsec, 1.1.1"

open System
open FParsec

let sampleWorkflow1 = "ex{x>10:one,m<20:two,a>30:R,A}"

type Part =
    {
        X: int
        M: int
        A: int
        S: int
    }

module Part =
    let create x m a s = { X = x; M = m; A = a; S = s }

    let px = pstring "x=" >>. pint32
    let pm = pstring "m=" >>. pint32
    let pa = pstring "a=" >>. pint32
    let ps = pstring "s=" >>. pint32
    let pComma = pchar ','
    let pLBrace = pchar '{'
    let pRBrace = pchar '}'
    let parse: Parser<_, unit> =
        parse {
            let! _ = pLBrace
            let! x = px .>> pComma
            let! m = pm .>> pComma
            let! a = pa .>> pComma
            let! s = ps
            let! _ = pRBrace
            return create x m a s
        }


type PartParam =
    | X
    | M
    | A
    | S

type Condition =
    | LessThan of PartParam * int
    | GreaterThan of PartParam * int

type RuleAction =
    | Accept
    | Reject
    | GotoWorkflow of string

type Rule = 
    {
        Condition: Condition option
        OnSuccess: RuleAction
        // On failure, goto next rule
    }

type Workflow = 
    {
        Name: string
        Rules: Rule list
    }

module Workflow =
    let create name rules = { Name = name; Rules = rules }

    // srz{m<3646:mxz,x<2707:A,bqs}
    let pName: Parser<_,unit> = many1CharsTill anyChar (pchar '{')

    let pPartParam: Parser<_,unit> = 
        [
            'x', X; 'm', M; 'a', A; 's', S
        ] |> List.map (fun (c, p) -> pchar c >>% p) |> choice

    let pCondition = 
        let pLessThan = 
            attempt (pPartParam .>> pchar '<' .>>. pint32 |>> fun (p, v) -> LessThan (p, v))
        let pGreaterThan = 
            attempt (pPartParam .>> pchar '>' .>>. pint32 |>> fun (p, v) -> GreaterThan (p, v))

        choice [pLessThan; pGreaterThan]

    let pRuleAction =
        let pAccept = attempt (pstring "A" >>% Accept)
        let pReject = attempt (pstring "R" >>% Reject)
        let pGotoWorkflow = attempt (many1Satisfy (fun c -> c >= 'a' && c <= 'z') |>> GotoWorkflow)

        choice [pAccept; pReject; pGotoWorkflow]

    let pRule: Parser<_,unit> =
        parse {
            let! condition = opt (pCondition .>> pchar ':')
            let! action = pRuleAction
            return { Condition = condition; OnSuccess = action }
        }
    let pRules: Parser<_,unit> = sepBy1 pRule (pchar ',')

    let parse: Parser<_,unit> =
        parse {
            let! name = pName
            let! rules = pRules
            let! _ = pchar '}'
            return create name rules
        }

    let eval (part: Part) (workflow: Workflow) =
        let rec loop (part: Part) (rules: Rule list) =
            match rules with
            | [] -> failwith "No matching rule found"
            | rule :: rules ->
                match rule.Condition with
                | None -> rule.OnSuccess
                | Some (LessThan (param, value)) ->
                    match param with
                    | X -> if part.X < value then rule.OnSuccess else loop part rules
                    | M -> if part.M < value then rule.OnSuccess else loop part rules
                    | A -> if part.A < value then rule.OnSuccess else loop part rules
                    | S -> if part.S < value then rule.OnSuccess else loop part rules
                | Some (GreaterThan (param, value)) ->
                    match param with
                    | X -> if part.X > value then rule.OnSuccess else loop part rules
                    | M -> if part.M > value then rule.OnSuccess else loop part rules
                    | A -> if part.A > value then rule.OnSuccess else loop part rules
                    | S -> if part.S > value then rule.OnSuccess else loop part rules

        loop part workflow.Rules

type Input =
    {
        Workflows: Workflow list
        Parts: Part list
    }

module Input =
    let create workflows parts = { Workflows = workflows; Parts = parts }

    let parse: Parser<_,unit> =
        parse {
            let! workflows = many1Till (Workflow.parse .>> newline) newline
            let! parts = many1Till (Part.parse .>> newline) eof
            return create workflows parts
        }

type Interpreter =
    {
        Workflows: Map<string, Workflow>
        In: Workflow
    }

type Range =
    {
        Start: int
        End: int
    }
    member this.Count = this.End - this.Start + 1

type XMASRanges =
    {
        XRange: Range
        MRange: Range
        ARange: Range
        SRange: Range
    }
    static member Start = 
        {
            XRange = { Start = 1; End = 4000 }
            MRange = { Start = 1; End = 4000 }
            ARange = { Start = 1; End = 4000 }
            SRange = { Start = 1; End = 4000 }
        }
    member this.Combinations =
        int64 this.XRange.Count * int64 this.MRange.Count * int64 this.ARange.Count * int64 this.SRange.Count

type SplitResult<'T> =
    | True of 'T
    | False of 'T
    | TrueFalse of 'T * 'T

module SplitResult =
    let map f x =
        match x with
        | True x -> True (f x)
        | False x -> False (f x)
        | TrueFalse (x, y) -> TrueFalse (f x, f y)

module Range = 
    let splitLT i (range: Range) =
        if range.Start >= i then False range
        elif i > range.End then True range
        else TrueFalse ({ range with End = i - 1 }, { range with Start = i })

    let splitGT i (range: Range) =
        if range.End <= i then False range
        elif i < range.Start then True range
        else TrueFalse ({ range with Start = i + 1 }, { range with End = i })

module XMASRanges =
    let splitBy condition ranges =
        match condition with
        | LessThan (X, i) -> 
            Range.splitLT i ranges.XRange
            |> SplitResult.map (fun x -> { ranges with XRange = x })
        | LessThan (M, i) ->
            Range.splitLT i ranges.MRange
            |> SplitResult.map (fun x -> { ranges with MRange = x })
        | LessThan (A, i) ->
            Range.splitLT i ranges.ARange
            |> SplitResult.map (fun x -> { ranges with ARange = x })
        | LessThan (S, i) ->
            Range.splitLT i ranges.SRange
            |> SplitResult.map (fun x -> { ranges with SRange = x })
        | GreaterThan (X, i) ->
            Range.splitGT i ranges.XRange
            |> SplitResult.map (fun x -> { ranges with XRange = x })
        | GreaterThan (M, i) ->
            Range.splitGT i ranges.MRange
            |> SplitResult.map (fun x -> { ranges with MRange = x })
        | GreaterThan (A, i) ->
            Range.splitGT i ranges.ARange
            |> SplitResult.map (fun x -> { ranges with ARange = x })
        | GreaterThan (S, i) ->
            Range.splitGT i ranges.SRange
            |> SplitResult.map (fun x -> { ranges with SRange = x })

module Interpreter =
    let create workflows = 
        let workflows = workflows |> List.map (fun w -> w.Name, w) |> Map.ofList
        { 
            Workflows = workflows
            In = workflows.["in"]
        }

    let eval (interpreter: Interpreter) (part: Part) =
        let rec processWorkflows (part: Part) (workflow: Workflow) =
            match Workflow.eval part workflow with
            | Accept -> Some part
            | Reject -> None
            | GotoWorkflow name -> 
                match interpreter.Workflows.TryFind name with
                | None -> failwithf "Workflow %s not found" name
                | Some workflow -> processWorkflows part workflow

        processWorkflows part interpreter.In

    let evalRanges (interpreter: Interpreter) =
        let range = XMASRanges.Start
        let rec ruleLoop (range: XMASRanges) (rules: Rule list) (successRanges) =
            match rules with
            | [] -> failwith "Last rule should always succeed"
            | rule :: rules ->
                match rule.Condition, rule.OnSuccess with
                | None, Accept -> range :: successRanges
                | None, Reject -> successRanges
                | None, GotoWorkflow name -> ruleLoop range (interpreter.Workflows.[name].Rules) successRanges
                | Some condition, Accept ->
                    match XMASRanges.splitBy condition range with
                    | True range -> range :: successRanges
                    | False range -> ruleLoop range rules successRanges
                    | TrueFalse (range1, range2) -> ruleLoop range2 rules (range1 :: successRanges)
                | Some condition, Reject ->
                    match XMASRanges.splitBy condition range with
                    | True _ -> successRanges
                    | False range -> ruleLoop range rules successRanges
                    | TrueFalse (_, range2) -> ruleLoop range2 rules successRanges
                | Some condition, GotoWorkflow name ->
                    match XMASRanges.splitBy condition range with
                    | True range -> ruleLoop range (interpreter.Workflows.[name].Rules) successRanges
                    | False range -> ruleLoop range rules successRanges
                    | TrueFalse (range1, range2) -> 
                        ruleLoop range1 (interpreter.Workflows.[name].Rules) []
                        @ ruleLoop range2 rules successRanges

        ruleLoop range interpreter.In.Rules []

let sampleInput = """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"""

let inputSample = 
    match run Input.parse sampleInput with
    | Success (result, _, _) -> result
    | Failure (msg, _, _) -> failwith msg


let part1Result (input: Input) = 
    let interpreter = Interpreter.create input.Workflows
    input.Parts
    |> List.choose (Interpreter.eval interpreter)
    |> List.sumBy (fun p -> p.X + p.M + p.A + p.S)

let part1ResultSample = part1Result inputSample

open System.IO
// Part1

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let parsedInput = 
    match run Input.parse input with
    | Success (result, _, _) -> result
    | Failure (msg, _, _) -> failwith msg

let part1Answer = part1Result parsedInput


type OptimiserState =
    {
        Trivial: Map<string, RuleAction>
        Workflows: Map<string, Workflow>
    }

module OptimiserState =
    let ofInput (x: Input) =
        {
            Trivial = Map.empty
            Workflows = x.Workflows |> List.map (fun w -> w.Name, w) |> Map.ofList
        }

    let tryPop (x: OptimiserState) =
        match Seq.tryHead x.Workflows with
        | Some (KeyValue(name, workflow)) -> 
            let workflows = x.Workflows.Remove(name)
            Some (workflow, { x with Workflows = workflows })
        | None -> None

    let (|AlwaysAccept|_|) (rule: Rule) =
        match rule.Condition, rule.OnSuccess with
        | None, Accept -> Some rule
        | _ -> None
    let (|MaybeAccept|_|) (rule: Rule) =
        match rule.Condition, rule.OnSuccess with
        | None, Accept 
        | Some _, Accept -> Some rule
        | _ -> None
    let (|AlwaysReject|_|) (rule: Rule) =
        match rule.Condition, rule.OnSuccess with
        | None, Reject -> Some rule
        | _ -> None
    let (|MaybeReject|_|) (rule: Rule) =
        match rule.Condition, rule.OnSuccess with
        | None, Reject 
        | Some _, Reject -> Some rule
        | _ -> None

        
    let rec collapseLoop didChange (rules: Rule list) =
        let collapseRules (rules: Rule list) =
            match rules with
            | AlwaysAccept rule :: MaybeAccept _ :: rules -> true, rule :: rules
            | AlwaysReject rule :: MaybeReject _ :: rules -> true, rule :: rules
            | _ -> false, rules

        match collapseRules rules with
        | true, rules -> collapseLoop true rules
        | false, rules -> didChange, List.rev rules

    let substitute (trivial: Map<string, RuleAction>) (rules: Rule list) =
        let rec loop didChange (rules: Rule list) (acc: Rule list) =
            match rules with
            | [] -> didChange, List.rev acc
            | rule :: rules ->
                match rule.OnSuccess with
                | GotoWorkflow name ->
                    match trivial.TryFind name with
                    | None -> loop didChange rules (rule :: acc)
                    | Some action -> loop true rules ({ rule with OnSuccess = action } :: acc)
                | _ -> loop didChange rules (rule :: acc)

        loop false rules []

    let substituteLoop (x: OptimiserState) =
        ((false, x.Workflows), x.Workflows)
        ||> Map.fold (fun (didChange, workflows) name workflow ->
            match substitute x.Trivial workflow.Rules with
            | true, rules -> true, workflows.Add(name, { workflow with Rules = rules })
            | false, _ -> didChange, workflows
        )
        |> function
        | true, workflows -> true, { x with Workflows = workflows }
        | false, _ -> false, x

    let (|Trivial|_|) (rule: Rule) =
        match rule.Condition, rule.OnSuccess with
        | None, Reject -> Some (rule.OnSuccess)
        | None, Accept -> Some (rule.OnSuccess)
        | _ -> None

    let rec optimise (x: OptimiserState): OptimiserState =

        let rec loop didChange (x: OptimiserState) (acc: Map<string, Workflow>) (workflow: Workflow) =
            match collapseLoop false (List.rev workflow.Rules) with
            | true, rules -> 
                match rules with
                | [ Trivial action ] -> 
                    let x = { x with Trivial = x.Trivial.Add(workflow.Name, action) }
                    match tryPop x with
                    | Some (workflow, x) -> loop true x acc workflow
                    | None -> didChange, { x with Workflows = acc }
                | _ ->
                    let workflow = { workflow with Rules = rules }
                    let acc = acc.Add(workflow.Name, workflow)
                    match tryPop x with
                    | Some (workflow, x) -> loop true x acc workflow
                    | None -> didChange, { x with Workflows = acc }
            | false, _ ->
                let acc = acc.Add(workflow.Name, workflow)
                match tryPop x with
                | Some (workflow, x) -> loop didChange x acc workflow
                | None -> didChange, { x with Workflows = acc }
        
        match tryPop x with
        | Some (workflow, x) -> 
            match loop false x Map.empty workflow with
            | true, x -> 
                match substituteLoop x with
                | true, x -> optimise x
                | false, x -> x
            | false, x -> x
        | None -> failwith "Empty input optimiser"

    let printAction =
        function
        | Accept -> "A"
        | Reject -> "R"
        | GotoWorkflow name -> name

    let printParam =
        function
        | X -> "x"
        | M -> "m"
        | A -> "a"
        | S -> "s"

    let printRule (rule: Rule) =
        match rule.Condition with
        | None -> sprintf "%s" (printAction rule.OnSuccess)
        | Some (LessThan (param, value)) -> sprintf "%s<%d:%s" (printParam param) value (printAction rule.OnSuccess)
        | Some (GreaterThan (param, value)) -> sprintf "%s>%d:%s" (printParam param) value (printAction rule.OnSuccess)
        
    let print (x:OptimiserState) =

        for KeyValue(name, trivial) in x.Trivial do
            printfn "%s{%s}" name (printAction trivial)

        for KeyValue(name, workflow) in x.Workflows do
            printfn "%s{%s}" name (String.Join(",", workflow.Rules |> List.map printRule))

    let toInterpreter (x: OptimiserState) =
        x.Workflows
        |> Map.values
        |> List.ofSeq
        |> Interpreter.create

let tee f x = f x; x

inputSample 
|> OptimiserState.ofInput
|> OptimiserState.optimise
|> tee OptimiserState.print
|> OptimiserState.toInterpreter
|> Interpreter.evalRanges
|> List.sumBy (fun x -> x.Combinations)

// Part 2
parsedInput
|> OptimiserState.ofInput
|> OptimiserState.optimise
|> tee OptimiserState.print
|> OptimiserState.toInterpreter
|> Interpreter.evalRanges
|> List.sumBy (fun x -> x.Combinations)

#time "on"
for i in 1..1000 do 
    parsedInput
    |> OptimiserState.ofInput
    // |> OptimiserState.optimise
    // |> tee OptimiserState.print
    |> OptimiserState.toInterpreter
    |> Interpreter.evalRanges
    |> List.sumBy (fun x -> x.Combinations)
    |> ignore
#time "off"

#time "on"
for i in 1..1000 do 
    parsedInput
    |> OptimiserState.ofInput
    |> OptimiserState.optimise
    // |> tee OptimiserState.print
    |> OptimiserState.toInterpreter
    |> Interpreter.evalRanges
    |> List.sumBy (fun x -> x.Combinations)
    |> ignore
#time "off"

// Turns out the optimiser is not needed
