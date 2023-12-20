module AdventOfCode23.Day20

open AdventOfCode23.Common
open System.Text.RegularExpressions

type Pulse =
    | No
    | Low
    | High

[<AbstractClass>]
type Module(name: string, destinations: string seq) =
    member __.Name = name
    member __.Destinations = destinations |> List.ofSeq
    abstract member UpdateAndGetPulse: Pulse -> string -> Pulse
    abstract member AnswerPulse: Pulse -> string -> (string * Pulse * string) list
    default this.AnswerPulse (pulse: Pulse) (origin: string) =
        let pulse = this.UpdateAndGetPulse pulse origin
        if pulse = No then
            []
        else
            this.Destinations |> List.map (fun destination -> this.Name, pulse, destination)
    override this.ToString () =
        sprintf "%s (-> %s)" this.Name (System.String.Join(", ", this.Destinations))

type Broadcaster(name: string, destinations: string seq) =
    inherit Module(name, destinations)
    override this.UpdateAndGetPulse (pulse: Pulse) (_: string) = pulse

type FlipFlop(name: string, destinations: string seq) =
    inherit Module(name, destinations)
    let mutable isOn = false
    override this.UpdateAndGetPulse (pulse: Pulse) (_: string) =
        if pulse = High then
            No
        else
            isOn <- not isOn
            if isOn then High else Low

type Conjunction(name: string, destinations: string seq) =
    inherit Module(name, destinations)
    let mutable lastPulse = Map.empty<string, Pulse>
    override this.UpdateAndGetPulse (pulse: Pulse) (origin: string) =
        lastPulse <- lastPulse |> Map.add origin pulse
        if lastPulse |> Map.forall (fun _ pulse -> pulse = High) then Low else High
    member __.ConnectInput (origin: string) =
        lastPulse <- lastPulse |> Map.add origin Low

let main (lines: string list) =
    let (|RX|_|) (regex: string) (str: string) =
        match Regex(regex).Match str with
        | m when m.Success -> Some (List.tail [ for x in m.Groups -> x.Value ])
        | _ -> None
    let (|Destinations|_|) (str: string) = str.Split(", ") |> Array.map (fun str -> str.Trim()) |> Some

    let makeNetwork (lines: string list) =
        let network =
            lines |> List.map (fun line ->
                match line with
                | RX @"(broadcaster) -> (.+)" [ name; Destinations destinations; ] -> Broadcaster(name, destinations) :> Module
                | RX @"%(.+) -> (.+)" [ name; Destinations destinations; ] -> FlipFlop(name, destinations) :> Module
                | RX @"&(.+) -> (.+)" [ name; Destinations destinations; ] -> Conjunction(name, destinations) :> Module
                | _ -> failwith <| sprintf "Oops... Unknown format '%s'" line
            ) |> List.map (fun m -> m.Name, m) |> Map.ofList
        network |> Map.values
        |> Seq.map (fun mOrigin ->
            mOrigin.Destinations |> List.choose (fun destination ->
                match network |> Map.tryFind destination with
                | Some m when (m :? Conjunction) -> Some (m :?> Conjunction)
                | _ -> None
            ) |> List.map (fun mDestination -> mOrigin, mDestination)
        ) |> Seq.collect id |> Seq.iter (fun (input, conjunction) -> conjunction.ConnectInput input.Name)
        network

    let input = [ ("button", Low, "broadcaster") ]
    let output = "rx"

    let solve1 (network: Map<string, Module>) =
        let addTpl (a: int * int) (b: int * int) = (fst a) + (fst b), (snd a) + (snd b)
        let rec loop (lows: int, highs: int) (queue: (string * Pulse * string) list) =
            match queue with
            | [] -> lows, highs
            | (origin, pulse, destination) :: tail ->
                let lows = lows + if pulse = Low then 1 else 0
                let highs = highs + if pulse = High then 1 else 0
                match network |> Map.tryFind destination with
                | Some m -> loop (lows, highs) (tail @ (m.AnswerPulse pulse origin))
                | None -> loop (lows, highs) tail
        let lows, highs =
            seq { 1..1000 } |> Seq.fold (fun (tpl) _ -> addTpl tpl (loop (0, 0) input)) (0, 0)
        lows * highs
    printfn "%d" (solve1 <| makeNetwork lines)

    let solve2 (network: Map<string, Module>) =
        let finalModule =
            network |> Map.values |> Seq.find (fun m -> m.Destinations |> List.contains output)
        let mutable sources =
            network |> Map.values
            |> Seq.choose (fun m -> if m.Destinations |> List.contains finalModule.Name then Some m.Name else None)
            |> Seq.map (fun name -> name, 0) |> Map.ofSeq

        let rec loop (n: int) (queue: (string * Pulse * string) list) =
            match queue with
            | [] -> loop (n + 1) input
            | (origin, pulse, destination) :: tail ->
                if destination = finalModule.Name && pulse = High then
                    sources <- sources |> Map.add origin n
                    if sources |> Map.forall (fun _ v -> v > 0) then
                        sources |> Map.values |> Seq.map (fun v -> v |> uint64) |> List.ofSeq |> Math.lcm
                    else
                        loop n tail
                else
                    match network |> Map.tryFind destination with
                    | Some m -> loop n (tail @ (m.AnswerPulse pulse origin))
                    | None -> loop n tail
        loop 1 input
    printfn "%d" (solve2 <| makeNetwork lines)
