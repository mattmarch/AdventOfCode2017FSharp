module Day1

open System.IO

let input = File.ReadAllText("Day1.txt") |> Seq.map (fun x -> int x - int '0')

let offsetBy1AndZip sequence =
  Seq.head sequence 
  |> Seq.singleton 
  |> Seq.append (Seq.tail sequence)
  |> Seq.zip sequence

let offsetByHalfAndZip sequence = 
  let offsetAmount = Seq.length sequence / 2
  let firstHalf = sequence |> Seq.take offsetAmount
  let secondHalf = sequence |> Seq.skip offsetAmount
  Seq.zip sequence (Seq.append secondHalf firstHalf)

let sumOfEquals sequence: int =
  sequence 
  |> Seq.filter (fun (a, b) -> a = b) 
  |> Seq.sumBy (fun (a, _) -> a)

let solve (sequence: int seq) = 
  printfn "Part 1: %i" (sequence |> offsetBy1AndZip |> sumOfEquals)
  printfn "Part 2: %i" (sequence |> offsetByHalfAndZip |> sumOfEquals)
