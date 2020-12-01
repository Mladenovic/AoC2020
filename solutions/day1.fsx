open System.IO

let input = 
    File.ReadAllLines @"input\Day1\input.txt"
    |> Array.map int

let pairs = seq {
    for i in [0 .. input.Length - 1] do
        for j in [i .. input.Length - 1] do 
            yield if input.[i] + input.[j] = 2020 then Some (i,j) else None
}

let solution = 
    pairs 
    |> List.ofSeq 
    |> List.choose id
    |> List.map (fun (x, y) -> x * y)