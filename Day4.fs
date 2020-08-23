module Day4

open System
open System.IO

let input = File.ReadLines("Day4.txt") 
            |> Seq.map (fun line -> (line.Split ' ') |> seq)

let noRepeats passphrase =
  Seq.distinct passphrase 
  |> Seq.length = Seq.length passphrase

let noAnagrams (passphrase: string seq) = 
  passphrase
  |> Seq.map (Seq.sort >> String.Concat)
  |> Seq.distinct
  |> Seq.length = Seq.length passphrase

let solve passphrases =
  printfn "Part 1: %i" (passphrases |> Seq.filter noRepeats |> Seq.length)
  printfn "Part 2: %i" (passphrases |> Seq.filter noAnagrams |> Seq.length)
  