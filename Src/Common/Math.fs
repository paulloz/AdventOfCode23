namespace AdventOfCode23.Common

module Math =
    let rec gcd (a: uint64) (b: uint64) = if b = 0UL then a else gcd b (a % b)

    let rec lcm (l: uint64 list) =
        let lcm' (a: uint64) (b: uint64) = (a * b) / (gcd a b)
        match l with
        | [] -> 1UL
        | [ a; b ] -> lcm' a b
        | head :: tail -> lcm' head (lcm tail)

