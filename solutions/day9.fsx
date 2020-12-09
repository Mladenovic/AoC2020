open System.IO

let input =
    File.ReadAllLines @"input\Day9\input.txt"
    |> Array.map int64

let preamble = 25

let getInvalid index =
    let firstValid =
        seq {
            for i in (index - preamble) .. (index - 1) do
                for j in i .. input.Length - 1 do
                    if input.[i] + input.[j] = input.[index] then Some input.[index] else None
        }
        |> Seq.tryPick id

    match firstValid with
    | Some _ -> None
    | None -> Some input.[index]

let solution1 =
    seq { 25 .. input.Length - 1 }
    |> Seq.tryPick getInvalid

let s1 = 542529149L

let smallestAndLargestInRangeSum (input: int64 []) lowIdx highIdx =
    printf ""
    let subArray =
        Array.sub input lowIdx (highIdx - lowIdx + 1)

    Array.min subArray + Array.max subArray

let findNumber (input: int64 []) s1 index =
    let possibleUpperOfSome =
        seq { index .. input.Length - 1 }
        |> Seq.scan (fun state el -> state + input.[el]) 0L
        |> Seq.skip 1
        |> Seq.mapi (fun i el -> if el > s1 || el < s1 then None else Some i)
        |> Seq.tryPick id

    match possibleUpperOfSome with
    | Some idx -> Some(smallestAndLargestInRangeSum input index (index + idx))
    | None -> None

let solution2 =
    seq { 0 .. input.Length - 1 }
    |> Seq.map (findNumber input s1)
    |> Seq.tryPick id
