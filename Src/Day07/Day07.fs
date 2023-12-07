module AdventOfCode23.Day07

type HandType =
    | HighCard = 0
    | OnePair = 1
    | TwoPair = 2
    | ThreeOfAKind = 3
    | FullHouse = 4
    | FourOfAKind = 5
    | FiveOfAKind = 6

type Hand =
    {
        Cards: char array;
        Rank: HandType;
    }

let reduceWinnings (hands: (Hand * int) list) =
    hands |> List.mapi (fun (i: int) (_, bid: int) -> (i + 1) * bid) |> List.sum

type HandManager() =
    abstract ValueFromCard : char -> int
    abstract HandTypeFromCards : char array -> HandType

    default __.ValueFromCard (card: char) =
        match card with
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | '5' -> 5
        | '6' -> 6
        | '7' -> 7
        | '8' -> 8
        | '9' -> 9
        | 'T' -> 10
        | 'J' -> 11
        | 'Q' -> 12
        | 'K' -> 13
        | 'A' -> 14
        | _ -> failwith "Oops"

    default __.HandTypeFromCards (cards: char array) =
        let groups =
            cards
                |> Array.groupBy id
                |> Array.map (fun (_, cards: char array) -> cards.Length)
                |> Array.sortDescending
        match groups with
        | [|5|] -> HandType.FiveOfAKind
        | [|4;1|] -> HandType.FourOfAKind
        | [|3;2|] -> HandType.FullHouse
        | [|3;1;1|] -> HandType.ThreeOfAKind
        | [|2;2;1|] -> HandType.TwoPair
        | [|2;1;1;1|] -> HandType.OnePair
        | _ -> HandType.HighCard

    member this.CreateHand (cardsRepresentation: string) =
        let cards = cardsRepresentation.Trim() |> Array.ofSeq
        let rank = this.HandTypeFromCards(cards)
        { Cards = cards; Rank = rank }

    member this.CreateHands (lines: string list) =
        lines |> List.map (fun (line: string) -> line.Split(" "))
              |> List.map (fun (line: string array) -> line.[0] |> this.CreateHand, line.[1].Trim() |> int)

    member this.RankHands (hands: (Hand * int) list) =
        let sorter (a: Hand, _) (b: Hand, _) =
            let rec loop (cardPairs: (char * char) list) =
                match cardPairs with
                | (cardA, cardB) :: tail when cardA = cardB -> loop tail
                | (cardA, cardB) :: _ -> (cardA |> this.ValueFromCard) - (cardB |> this.ValueFromCard)
                | [] -> 0
            b.Cards |> Array.zip a.Cards |> List.ofArray |> loop
        hands |> List.sortWith sorter |> List.sortBy (fun (hand: Hand, _) -> hand.Rank)

    member this.CreateAndRankHands (lines: string list) =
        lines |> this.CreateHands |> this.RankHands

type JokerHandManager() =
    inherit HandManager()

    override __.ValueFromCard (card: char) =
        match card with
        | 'J' -> 1
        | _ -> base.ValueFromCard(card)

    override __.HandTypeFromCards (cards: char array) =
        let groups =
            cards
                |> Array.filter (fun (c: char) -> c <> 'J')
                |> Array.groupBy id
                |> Array.map (fun (_, cards: char array) -> cards.Length)
                |> Array.sortDescending
        match groups with
        | [|_|] | [||] -> HandType.FiveOfAKind
        | [|4;1|] | [|3;1|] | [|2;1|] | [|1;1|] -> HandType.FourOfAKind
        | [|3;2|] | [|2;2|] -> HandType.FullHouse
        | [|3;1;1|] | [|2;1;1|] | [|1;1;1|] -> HandType.ThreeOfAKind
        | [|2;2;1|] -> HandType.TwoPair
        | [|2;1;1;1|] | [|1;1;1;1|] -> HandType.OnePair
        | _ -> HandType.HighCard

let main (lines: string list) =
    let step1 = new HandManager()
    printfn "Total winnings: %d" (lines |> step1.CreateAndRankHands |> reduceWinnings)

    let step2 = new JokerHandManager()
    printfn "Total winnings (using Joker cards): %d" (lines |> step2.CreateAndRankHands |> reduceWinnings)
