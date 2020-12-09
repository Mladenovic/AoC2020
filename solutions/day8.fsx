open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type InstructionType =
    | Jump
    | Accumulate
    | NoOperation
    | Terminate
    
type Instruction =
    { InstructionType: InstructionType
      Amount: int }
    
let parseInstructionType str =
    match str with
    | "acc" -> Accumulate
    | "jmp" -> Jump
    | "nop" -> NoOperation
    | "ter" -> Terminate 
    | _ -> failwith "wrong instruction format"
    
let parseInstruction str =
    match str with
    | Regex "(acc|jmp|nop|ter) ([\+|\-][0-9]+)" [instructionTypeStr; amountStr] ->
        { InstructionType = parseInstructionType instructionTypeStr
          Amount = int amountStr }    
    | _ -> failwith "wrong instruction format"
    
let input = 
    File.ReadAllLines @"input\Day8\input.txt"
    |> Array.map parseInstruction



let rec execute acc (executed:Set<int>) insNr (input: Instruction [])=
    if executed.Contains insNr
    then
        false, acc
    else
        let instruction = input.[insNr]
        match instruction.InstructionType with
        | NoOperation -> execute acc (executed.Add insNr) (insNr + 1) input
        | Jump -> execute acc (executed.Add insNr) (insNr + instruction.Amount) input
        | Accumulate -> execute (acc + instruction.Amount) (executed.Add insNr) (insNr + 1) input
        | Terminate ->
            true, acc
        
let solution1 = execute 0 Set.empty 0 input

let flipInstruction instruction =
    match instruction.InstructionType with
    | Jump -> { instruction with InstructionType = NoOperation }
    | NoOperation -> { instruction with InstructionType = Jump }
    | _ -> instruction

let changedInputs (input: Instruction []) =
    seq {
        for i in 0 ..(input.Length - 1) do
            let instruction = input.[i]
            match instruction.InstructionType with
            | Jump | NoOperation ->
                Some (input |> Array.mapi (fun idx el -> if idx = i then flipInstruction el else el))
            | _ -> None
    }
    |> Seq.choose id
    
let solution2 =
    changedInputs input
    |> Seq.map (execute 0 Set.empty 0)
    |> Seq.filter fst
    |> Seq.head