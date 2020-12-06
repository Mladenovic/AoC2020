open System.IO

let countYeses (question: string) =
    question.Replace("\n", "").Trim()
    |> Set.ofSeq
    |> Set.count

let countSameAnswers (question: string) =
    question.Split([| "\n" |], System.StringSplitOptions.None)
    |> Array.map (fun x -> x |> Set.ofSeq)
    |> Set.intersectMany
    |> Set.count

let input =
    (File.ReadAllText @"input\Day6\input.txt")
        .Split([| "\n\n" |], System.StringSplitOptions.None)

let solution =
    input |> Array.map countYeses |> Array.sum

let solution2 =
    input |> Array.map countSameAnswers |> Array.sum