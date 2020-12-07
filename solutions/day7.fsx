open System
open System.IO
open System.Text.RegularExpressions

let tryParseInt (str:string) =
    try
        Some (int str)
    with :? FormatException ->
        None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Bag = { Color: string }

type ContainedBag = { Bag: Bag; Number: int }

let parseContainedBags (containedBagsStr: string) =
    containedBagsStr.Split ','
    |> Array.map (fun x ->
        match x with
        | Regex "^ *([0-9]+) (.*?) bags*\.*" [ number; color ] ->
            match tryParseInt number with
            | Some nr -> Some { Number = nr; Bag = { Color = color }}
            | _ ->
                printf "something went wrong"
                None
        | _ ->
            printf "something went wrong"
            None)
    |> Array.choose id
    

let parseSingleInput (str: string) =
    let [| containerBagStr; containedBagsStr |] =
        str.Split([| " bags contain " |], StringSplitOptions.None)

    let containerBag = { Color = containerBagStr }
    containerBag, parseContainedBags containedBagsStr |> Set.ofArray

let input =
    File.ReadAllLines @"input\Day7\input.txt"
    |> Array.map parseSingleInput
    |> Map.ofArray

let sol1Input =
    File.ReadAllLines @"input\Day7\input.txt"
    |> Array.map parseSingleInput
    |> Array.map (fun (x,y) -> x, (y |> Set.map (fun z -> z.Bag)))
    |> Map.ofArray

let shinyGold = { Color = "shiny gold" }

let rec containsShinyGold bag =
    let contained = sol1Input.[bag]
    if contained.Contains shinyGold then true
    else contained |> Set.map (fun x -> containsShinyGold x) |> Set.exists id
    
let sol1 =
    input
    |> Map.toSeq
    |> Seq.map (fst >> containsShinyGold)
    |> Seq.filter id
    |> Seq.length
    
    
let rec calculateBagNumber bag =
    let contained = input.[bag]
    if contained.IsEmpty then 1
    else 1 +
         (contained |> Set.toSeq |> Seq.sumBy (fun x -> x.Number * calculateBagNumber x.Bag))
    
let soulution2 = calculateBagNumber shinyGold - 1