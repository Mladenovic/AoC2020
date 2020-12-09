open System.IO
open System.Text.RegularExpressions

type PasswordPolicy =
    { Low: int
      High: int
      Character: char
      Password: string }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
   
let parseInputLine line =
    match line with
    | Regex @"([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" [ low; high; character; password ] ->
        Some { Low = int low; High = int high; Character = char character; Password = password }
    | _ -> None
    
let countChar str ch =
    str
    |> Seq.filter ((=) ch)
    |> Seq.length
    
let isValid passwordPolicy =
    let count = countChar passwordPolicy.Password passwordPolicy.Character
    count >= passwordPolicy.Low && count <= passwordPolicy.High

let xor a b = (a || b) && not (a && b)
    
let checkCharacter (str:string) (idx:int) chr =
    try
        str.[idx - 1] = chr
    with
        | _ -> false
    
let isValidPart2 passwordPolicy =
    xor
        (checkCharacter passwordPolicy.Password passwordPolicy.Low passwordPolicy.Character)
        (checkCharacter passwordPolicy.Password passwordPolicy.High passwordPolicy.Character)
     
    
    
let input = 
    File.ReadAllLines @"input\Day2\input.txt"
    |> Array.choose parseInputLine

let part1Solution =
    input
    |> Array.filter isValid
    |> Array.length
    
let part2Soulution =
    input
    |> Array.filter isValidPart2
    |> Array.length