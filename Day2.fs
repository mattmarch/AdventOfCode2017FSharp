module Day2

open System.IO

let input = File.ReadLines("Day2.txt") |> Seq.map (fun line -> (line.Split '\t') |> Seq.map int)

let lineDifference line = (Seq.max line) - (Seq.min line)

type ExactDivisionResult = ExactlyDivisible of int | NotDivisible

let exactDivision a b = 
  match a % b with
    | 0 -> Some (a / b)
    | _ -> None

let lineWithout x line = line |> Seq.filter (fun y -> y <> x)

let matchingDivisor line x = line |> lineWithout x |> Seq.tryPick (exactDivision x)

let lineDivision line = line |> Seq.pick (matchingDivisor line)

let solve spreadsheet =
  printfn "Part 1: %i" (spreadsheet |> Seq.sumBy lineDifference)
  printfn "Part 2: %i" (spreadsheet |> Seq.sumBy lineDivision)