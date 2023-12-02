module AdventOfCode23.Day02

open System.Text.RegularExpressions

type Color = Red | Green | Blue
type Game =
    {
        ID: int;
        Hands: (Color * int) array array;
    }

let extractGameData (game: string) =
    game.Split(';') |> Array.map (fun hand ->
        hand.Split(',') |> Array.map (fun value ->
            let splitValue = value.Trim().Split(' ')
            (match splitValue[1] with
                | "red" -> Red
                | "green" -> Green
                | "blue" -> Blue
                | _ -> raise(System.Exception(""))
            , splitValue[0] |> int)
        ) 
    )

let filterHandsForStep1 (game: Game) =
    game.Hands |> Array.forall (fun hand ->
        hand |> Array.forall (fun (color, n) ->
            match color with
            | Red -> n <= 12
            | Green -> n <= 13
            | Blue -> n <= 14
    ))

let extractGamePowers (game: Game) =
    game.Hands
        |> Seq.collect id
        |> Seq.groupBy (fun (color: Color, n: int) -> color)
        |> Seq.map (fun (key: Color, values: (Color * int) seq) ->
            values |> Seq.map (fun (c: Color, n: int) -> n) |> Seq.max
        )
        |> Seq.reduce (*)

let main (lines: string list) =
    let r = new Regex(@"Game (\d+): ((.+);?)+")
    let games = lines |> List.choose (fun line ->
        let m = r.Match(line)
        if m.Success then
            Some({ID = m.Groups[1].Value |> int; Hands = extractGameData(m.Groups[2].Value)})
        else
            None
    )
    printfn "Games passing step 1: %d" (games |> List.filter filterHandsForStep1 |> List.sumBy (fun game -> game.ID))
    printfn "Total games power: %d" (games |> List.map extractGamePowers |> List.reduce (+))
