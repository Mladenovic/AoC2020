open System
open System.IO
open System.Text.RegularExpressions

type Passport =
    { Byr: string option
      Iyr: string option
      Eyr: string option
      Hgt: string option
      Hcl: string option
      Ecl: string option
      Pid: string option
      Cid: string option }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None



let tryParseInt (str:string) =
    try
        Some (int str)
    with :? FormatException ->
        None
        
let validateYear code low high pass =
    let pattern = code + ":([1-2][0-9][0-9][0-9])\\b"
    match pass with
    | Regex pattern [ byr ] ->
        match tryParseInt byr with
        | Some x -> x >= low && x <= high
        | None -> false
    | _ -> false
    
let validateByr = validateYear "byr" 1920 2002
let validateIyr = validateYear "iyr" 2010 2020
let validateEyr = validateYear "eyr" 2020 2030

let validateHgt pass =
    let patternCm = "hgt:([0-9]+)cm\\b"
    let patternIn = "hgt:([0-9]+)in\\b"
    match pass with
    | Regex patternCm [ hgt ] ->
        match tryParseInt hgt with
        | Some x -> x >= 150 && x <= 193
        | None -> false
    | Regex patternIn [ hgt ] ->
        match tryParseInt hgt with
        | Some x -> x >= 59 && x <= 76
        | None -> false
    | _ -> false

let validateHcl pass =
    let pattern = "hcl:#[a-f0-9]{6}\\b"
    match pass with
    | Regex pattern _ -> true
    | _ -> false
    
let validateEcl pass =
    let pattern = "ecl:amb|blu|brn|gry|grn|hzl|oth\\b"
    match pass with
    | Regex pattern _ -> true
    | _ -> false
let validatePid pass =
    let pattern = "pid:[0-9]{9}\\b"
    match pass with
    | Regex pattern _ -> true
    | _ -> false
    
let validatePassport pass =
    validateByr pass &&
    validateIyr pass &&
    validateEyr pass &&
    validateHgt pass &&
    validateHcl pass &&
    validateEcl pass &&
    validatePid pass

let input =
    (File.ReadAllText @"input\Day4\input.txt").Split([| "\n\n" |], System.StringSplitOptions.None)
  
let soulution1 =
    input
    |> Array.map (fun pass ->
        pass.Contains("byr:")
        && pass.Contains("iyr:")
        && pass.Contains("eyr:")
        && pass.Contains("hgt:")
        && pass.Contains("hcl:")
        && pass.Contains("ecl:")
        && pass.Contains("pid:"))
    |> Array.filter id
    |> Array.length
    
let soulution2 =
    input
    |> Array.map validatePassport
    |> Array.filter id
    |> Array.length