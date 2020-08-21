module Day2

open System.IO

let input = File.ReadLines("Day2.txt") |> Seq.map (fun line -> (line.Split '\t') |> Seq.map int)

let lineDifference line = (Seq.max line) - (Seq.min line)

let solve spreadsheet =
  printfn "Part 1: %i" (spreadsheet |> Seq.sumBy lineDifference)