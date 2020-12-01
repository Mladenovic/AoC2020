open System.IO

let input = 
    File.ReadAllLines @"input\Day1\input.txt"
    |> Array.map int

let pairs = seq {
    for i in [0 .. input.Length - 1] do
        for j in [i .. input.Length - 1] do 
            yield if input.[i] + input.[j] = 2020 then Some (input.[i] * input.[j]) else None
}

let solutionPart1 = 
    pairs
    |> Seq.choose id
    |> List.ofSeq         

let triples = seq {
    for i in [0 .. input.Length - 1] do
        for j in [i .. input.Length - 1] do
            for k in [j .. input.Length - 1] do
                yield if input.[i] + input.[j] + input.[k] = 2020 then Some (input.[i] * input.[j] * input.[k]) else None 
}

let solutionPart2 =
    triples
    |> Seq.choose id
    |> List.ofSeq
