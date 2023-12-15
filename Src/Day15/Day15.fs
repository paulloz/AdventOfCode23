module AdventOfCode23.Day15

open System.Text.RegularExpressions

type Lens =
    { Label: string; Length: int; }
    with override this.ToString() = sprintf "(%s=%d)" this.Label this.Length

type Instruction =
    | D of string
    | E of Lens
    static member R = new Regex(@"(?<label>\w+)(?<op>[=-])((?<==)(?<length>\d+))?", RegexOptions.Compiled) 
    static member Parse (input: string) =
        match Instruction.R.Match input with
        | m when not m.Success -> failwith <| sprintf "Invalid step '%s'" input
        | m ->
            match m.Groups.["op"].Value with
            | "-" -> D m.Groups.["label"].Value
            | "=" -> E { Label = m.Groups.["label"].Value; Length = m.Groups.["length"].Value |> int; }
            | op -> failwith <| sprintf "Invalid op '%s' in step '%s'" op input

type Hashmap(steps: string list) =
    let boxes: Lens list array = Array.create 256 []

    let tryFindInBox (boxIdx: int) (label: string) = boxes.[boxIdx] |> List.tryFindIndex (fun lens -> lens.Label = label)

    do
        let rec loop (steps: string list) =
            match steps with
            | head :: tail ->
                match Instruction.Parse head with
                | D label ->
                    let hash = Hashmap.MakeHash label
                    match tryFindInBox hash label with
                    | Some idx -> boxes.[hash] |> List.removeAt idx |> Array.set boxes hash
                    | None -> ()
                | E lens ->
                    let hash = Hashmap.MakeHash lens.Label
                    Array.set boxes hash <|
                        match tryFindInBox hash lens.Label with
                        | Some idx -> boxes.[hash] |> List.updateAt idx lens
                        | None -> boxes.[hash] @ [ lens ]
                loop tail
            | [] -> ()
        loop steps

    member __.ComputeFocusingPower() =
        let rec loop (boxIdx: int) (power: int) =
            if boxIdx >= 0 && boxIdx < boxes.Length then
                let boxPower =
                    boxes.[boxIdx]
                    |> List.mapi (fun slotIdx lens -> (boxIdx + 1) * (slotIdx + 1) * lens.Length)
                    |> List.sum
                loop (boxIdx + 1) power + boxPower
            else
                power
        loop 0 0

    override __.ToString() =
        let box2str (box: Lens list) = sprintf "[%s]" (System.String.Join("; ", box))
        System.String.Join("\n", boxes
            |> Array.filter (fun box -> box.Length > 0)
            |> Array.mapi (fun i box -> sprintf "%d: %s" i (box2str box))
        )

    static member MakeHash (value: string) =
        let rec loop (n: int) (value: char list) =
            match value with
            | [] -> n
            | head :: tail -> loop (((n + (int head)) * 17) % 256) tail
        value |> List.ofSeq |> loop 0

let main (lines: string list) =
    let steps = System.String.Concat(lines).Split(',') |> List.ofArray

    printfn "Sum of HASH results: %d" (steps |> List.map Hashmap.MakeHash |> List.sum)

    let hashmap = new Hashmap(steps)

    printfn "Focusing power after HASHMAP: %d" (hashmap.ComputeFocusingPower())
