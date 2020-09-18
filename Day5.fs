module Day5

open System.IO

let input = File.ReadLines "Day5.txt"
            |> Seq.map int
            |> Seq.toList

let incrementAtIndex index values =
  values |> List.mapi (fun i v -> if i = index then v + 1 else v)

let rec executeInstructions depth index (instructions: int list) =
  let nextIndex = instructions.[index] + index
  match nextIndex with
    | i when i < 0 || i >= List.length instructions -> depth + 1
    | _ -> executeInstructions (depth+1) nextIndex (incrementAtIndex index instructions)

// Part 2

let decrementAtIndex index values =
  values |> List.mapi (fun i v -> if i = index then v - 1 else v)

let rec executeInstructionsPartB depth index (instructions: int list) =
  let instruction = instructions.[index]
  let nextIndex = instruction + index
  let updatedInstructions = 
    if instruction >= 3 then 
      decrementAtIndex index instructions 
    else
      incrementAtIndex index instructions
  match nextIndex with
    | i when i < 0 || i >= List.length instructions -> depth + 1
    | _ -> executeInstructionsPartB (depth+1) nextIndex updatedInstructions


let solve instructions =  
  printfn "Part 1: %i" (executeInstructions 0 0 instructions)
  printfn "Part 2: %i" (executeInstructionsPartB 0 0 instructions)