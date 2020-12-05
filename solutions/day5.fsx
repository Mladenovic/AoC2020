open System
open System.IO

type Range =
    { Back: int
      Front: int
      Left: int
      Right: int }

type RowSplit =
    | Back
    | Front
    | Left
    | Right
    | Unknown

let maxRange =
    { Back = 0
      Front = 127
      Left = 0
      Right = 7 }

let splitRows range split =
    let halfRow = (range.Front - range.Back + 1) / 2
    let halfSeat = (range.Right - range.Left + 1) / 2

    match split with
    | Back ->
        { range with
              Back = range.Back + halfRow }
    | Front ->
        { range with
              Front = range.Front - halfRow }
    | Left ->
        { range with
              Right = range.Right - halfSeat }
    | Right ->
        { range with
              Left = range.Left + halfSeat }
    | Unknown -> failwith "wrong input"

let rowId range splitList =
    let seat = splitList |> Seq.fold splitRows range
    seat.Back * 8 + seat.Left

let parseCommand ch =
    match ch with
    | 'B' -> Back
    | 'F' -> Front
    | 'L' -> Left
    | 'R' -> Right
    | _ -> Unknown

let parseRow (rowStr: string) =
    rowStr
    |> Seq.map parseCommand

let input =
    File.ReadAllLines @"input\Day5\input.txt"
    |> Array.map (parseRow >> (rowId maxRange))

    
let solution1 =
    input
    |> Array.max

let minId = 1 * 8
let maxId = 127 * 8 - 1

let seatIdSet = input |> Set.ofArray

let checkSeat id =
    if seatIdSet.Contains id then false
    else (seatIdSet.Contains (id - 1)) && (seatIdSet.Contains (id + 1))
    
let solution2 =
    [minId .. maxId]
    |> List.map (fun id -> (checkSeat id), id)
    |> List.filter fst
    |> List.map snd
    |> List.head