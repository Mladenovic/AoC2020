open System.IO

let input =
    File.ReadAllLines @"input\Day3\input.txt"
    |> Array.map (fun ch -> ch |> Seq.map ((=) '#') |> Array.ofSeq)

let lineLenght = input |> Array.head |> Array.length

type State = { Column: int; Trees: int64 }

let initialState = { Column = 0; Trees = 0L }

let folder slope state (curr: bool []) =
    { Column = state.Column + slope
      Trees =
          state.Trees
          + if curr.[state.Column % lineLenght] then 1L else 0L }

let everyNth n (input: seq<_>) =
    input
    |> Seq.mapi (fun i el -> el, i)
    |> Seq.filter (fun (el, i) -> i % n = 0)
    |> Seq.map fst

let getSlope right down =
    everyNth down input
    |> Seq.fold (folder right) initialState

let solutionPart1 = getSlope 3 1
    
let solutionPart2 = (getSlope 1 1).Trees * (getSlope 3 1).Trees * (getSlope 5 1).Trees * (getSlope 7 1).Trees * (getSlope 1 2).Trees