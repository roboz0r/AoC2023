open System

let sample =
    """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

type HandType =
    | Five = 6
    | Four = 5
    | FullHouse = 4
    | Three = 3
    | TwoPair = 2
    | Pair = 1
    | High = 0

module Hand =

    // Highest to lowest A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2
    let order =
        [|
            '2'
            '3'
            '4'
            '5'
            '6'
            '7'
            '8'
            '9'
            'T'
            'J'
            'Q'
            'K'
            'A'
        |]

    let handType (cards: string) =
        let grouped =
            cards.ToCharArray()
            |> Array.countBy id
            |> Array.sortByDescending snd
            |> Array.toList

        match grouped with
        | [ (_, 5) ] -> HandType.Five
        | [ (_, 4); _ ] -> HandType.Four
        | [ (_, 3); (_, 2) ] -> HandType.FullHouse
        | (_, 3) :: _ -> HandType.Three
        | [ (_, 2); (_, 2); _ ] -> HandType.TwoPair
        | (_, 2) :: _ -> HandType.Pair
        | _ -> HandType.High

    let compare (cards1: string) (cards2: string) =
        let handType1 = handType cards1
        let handType2 = handType cards2

        if handType1 > handType2 then
            1
        elif handType1 < handType2 then
            -1
        else

            let rec compareCards i (cards1: string) (cards2: string) =
                if i < cards1.Length then
                    match Array.IndexOf(order, cards1[i]), Array.IndexOf(order, cards2[i]) with
                    | a, b when a > b -> 1
                    | a, b when a < b -> -1
                    | _ -> compareCards (i + 1) cards1 cards2
                else
                    0

            compareCards 0 cards1 cards2

[<CustomComparison; CustomEquality>]
type Hand =
    {
        Cards: string
        Bid: int
    }
    interface IComparable<Hand> with
        member this.CompareTo(other: Hand) = Hand.compare this.Cards other.Cards

    interface IComparable with
        member this.CompareTo(obj: obj) =
            match obj with
            | :? Hand as other -> (this :> IComparable<Hand>).CompareTo other
            | _ -> invalidArg "obj" "Must be of type Hand"

    override this.Equals(obj: obj) =
        match obj with
        | :? Hand as other -> (this :> IComparable<Hand>).CompareTo other = 0
        | _ -> false

    override this.GetHashCode() = hash this.Cards

let parse (line: string) =
    match line.Split(' ') with
    | [| cards; bid |] -> { Cards = cards; Bid = int bid }
    | _ -> failwith $"Invalid input {line}"

let sampleHands = sample.Split('\n') |> Array.map parse

let totalWinnings hands =
    hands
    |> Array.sort
    |> Array.indexed
    |> Array.fold (fun (totalRank) (rank, hand) -> totalRank + ((rank + 1) * hand.Bid)) 0
    |> printfn "Total winnings: %d"

totalWinnings sampleHands

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt"))

let inputHands =
    input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parse

totalWinnings inputHands


module Hand2 =

    // J becomes wild but it reordered lowest
    let order =
        [|
            'J'
            '2'
            '3'
            '4'
            '5'
            '6'
            '7'
            '8'
            '9'
            'T'
            'Q'
            'K'
            'A'
        |]

    let handType (cards: string) =
        let grouped =
            cards.ToCharArray()
            |> Array.countBy id
            |> Array.sortByDescending (fun (c, i) -> i * 100 + Array.IndexOf(order, c))
            |> Array.toList

        match grouped with
        | [ (_, 5) ] -> HandType.Five
        | [ ('J', 4); (_, 1) ] -> HandType.Five
        | [ (_, 4); ('J', 1) ] -> HandType.Five
        | [ (_, 4); _ ] -> HandType.Four
        | [ ('J', 3); (_, 2) ] -> HandType.Five
        | [ (_, 3); ('J', 2) ] -> HandType.Five
        | [ (_, 3); (_, 2) ] -> HandType.FullHouse
        | [ ('J', 3); (_, 1); (_, 1) ] -> HandType.Four
        | [ (_, 3); (_, 1); ('J', 1) ] -> HandType.Four
        | (_, 3) :: _ -> HandType.Three
        | [ (_, 2); ('J', 2); _ ] -> HandType.Four
        | [ (_, 2); (_, 2); ('J', 1) ] -> HandType.FullHouse
        | [ (_, 2); (_, 2); _ ] -> HandType.TwoPair
        | [ (_, 2); _; _; ('J', 1) ] -> HandType.Three
        | ('J', 2) :: _ -> HandType.Three
        | (_, 2) :: _ -> HandType.Pair
        | [ _; _; _; _; ('J', 1) ] -> HandType.Pair
        | _ -> HandType.High

    // Experiment with active patterns as alternative
    let (|J|_|) =
        function
        | ('J', i) -> Some i
        | _ -> None

    let (|C|) (_, i) = i


    let handType2 (cards: string) =
        let grouped =
            cards.ToCharArray()
            |> Array.countBy id
            |> Array.sortByDescending (fun (c, i) -> i * 100 + Array.IndexOf(order, c))
            |> Array.toList

        match grouped with
        | [ C 5 ] -> HandType.Five
        | J 4 :: _ -> HandType.Five

        | C 4 :: J 1 :: _ -> HandType.Five
        | C 4 ::  _ -> HandType.Four

        | J 3 :: C 2 :: _ -> HandType.Five
        | J 3 :: _ -> HandType.Four

        | C 3 :: x -> 
            match x with
            | [J 2] -> HandType.Five
            | [C 2] -> HandType.FullHouse
            | [_; J 1] -> HandType.Four
            | _ -> HandType.Three
        
        | [ C 2; J 2; _ ] -> HandType.Four
        | [ C 2; C 2; J 1 ] -> HandType.FullHouse
        | [ C 2; C 2; _ ] -> HandType.TwoPair
        | [ C 2; _; _; J 1 ] -> HandType.Three
        | J 2 :: _ -> HandType.Three
        | C 2 :: _ -> HandType.Pair
        | [ _; _; _; _; J 1 ] -> HandType.Pair
        | _ -> HandType.High

    let compare (cards1: string) (cards2: string) =
        let handType1 = handType cards1
        let handType2 = handType cards2

        if handType1 > handType2 then
            1
        elif handType1 < handType2 then
            -1
        else

            let rec compareCards i (cards1: string) (cards2: string) =
                if i < cards1.Length then
                    match Array.IndexOf(order, cards1[i]), Array.IndexOf(order, cards2[i]) with
                    | a, b when a > b -> 1
                    | a, b when a < b -> -1
                    | _ -> compareCards (i + 1) cards1 cards2
                else
                    0

            compareCards 0 cards1 cards2

[<CustomComparison; CustomEquality>]
type Hand2 =
    {
        Cards: string
        Bid: int
    }
    interface IComparable<Hand2> with
        member this.CompareTo(other: Hand2) = Hand2.compare this.Cards other.Cards

    interface IComparable with
        member this.CompareTo(obj: obj) =
            match obj with
            | :? Hand2 as other -> (this :> IComparable<Hand2>).CompareTo other
            | _ -> invalidArg "obj" "Must be of type Hand"

    override this.Equals(obj: obj) =
        match obj with
        | :? Hand2 as other -> (this :> IComparable<Hand2>).CompareTo other = 0
        | _ -> false

    override this.GetHashCode() = hash this.Cards

let parse2 (line: string) =
    match line.Split(' ') with
    | [| cards; bid |] -> { Cards = cards; Bid = int bid }
    | _ -> failwith $"Invalid input {line}"

let sampleHands2 = sample.Split('\n') |> Array.map parse2

let totalWinnings2 hands =
    hands
    |> Array.sort
    |> Array.indexed
    |> Array.fold (fun (totalRank) (rank, hand) -> totalRank + ((rank + 1) * hand.Bid)) 0
    |> printfn "Total winnings: %d"

totalWinnings2 sampleHands2

let inputHands2 =
    input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parse2

totalWinnings2 inputHands2
