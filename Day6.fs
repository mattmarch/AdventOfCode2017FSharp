module Day6

open System.IO

let input = 
  (File.ReadAllText "Day6.txt").Split("\t") 
  |> Seq.map int 
  |> Seq.toList

let maximum inputList = 
  let max = List.max inputList
  let index = inputList |> List.findIndex ((=) max)
  (max, index)

let withinRange rangeStart rangeLength value =
  value > rangeStart && value <= rangeStart + rangeLength

let shouldGetExtra index indexOfMax remainder length =
  let withinRangeWithExtra = withinRange indexOfMax remainder
  withinRangeWithExtra index || withinRangeWithExtra (index + length)

let distributeMaximum inputBanks =
  let length = List.length inputBanks
  let max, maxIndex = maximum inputBanks
  let dividend = max / length
  let remainder = max % length
  inputBanks 
  |> List.mapi (fun i value -> (if i = maxIndex then 0 else value) 
                                + dividend 
                                + (if shouldGetExtra i maxIndex remainder length then 1 else 0)
                )

let rec runDistributionCycles inputBanks previousStates =
  let nextState = distributeMaximum inputBanks
  if List.exists ((=) nextState) previousStates 
    then (List.length previousStates + 1, List.findIndex ((=) nextState) previousStates)
    else runDistributionCycles nextState (List.append previousStates [inputBanks])

let solve inputBanks =
  let totalCycles, previousIndex = runDistributionCycles inputBanks []
  printfn "Part 1: %i" totalCycles
  printfn "Part 2: %i" (totalCycles - previousIndex)