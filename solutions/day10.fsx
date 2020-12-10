open System.Collections.Generic
open System.IO

let input =
    File.ReadAllLines @"input\Day10\input.txt"
    |> Array.map uint64

let startState =
    match input |> Array.min with
    | 1UL -> (1UL, 1UL)
    | 3UL -> (0UL, 2UL)
    | _ -> (0UL, 1UL)

let countOnesAndTrees (input: uint64 []) =
    input
    |> Array.sort
    |> Array.pairwise
    |> Array.fold (fun state el ->
        let x, y = el
        let ones, trees = state
        match y - x with
        | 1UL -> ones + 1UL, trees
        | 3UL -> ones, trees + 1UL
        | _ -> state) startState

let solution1 =
    let o, t = countOnesAndTrees input
    o * t

let cache = Dictionary<uint64, uint64>()

let rec countPossibilities (start: uint64): uint64 =
    match cache.TryGetValue start with
    | true, v -> v
    | false, _ ->
        let possibleJumps =
            input
            |> Array.filter (fun x -> x > start && x <= start + 3UL)

        let result =
            match possibleJumps with
            | [||] -> 1UL
            | xs -> xs |> Array.sumBy (countPossibilities)

        cache.Add(start, result)
        result

let solution2 = countPossibilities 0UL
