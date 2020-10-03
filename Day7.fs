module Day7

open System.IO

type Name = string

type ParsedNode = {
  Name: Name;
  Weight: int;
  Holding: Name list;
}

let parseTopNode (line: string) =
  let splitLine = line.Split()
  let name = splitLine.[0]
  let weight = splitLine.[1]
                .Replace("(", "")
                .Replace(")", "")
                |> int
  name, weight        

let parseHolding (holdingLinePart: string) =
  holdingLinePart.Split(", ")
    |> Array.toList

let parseLine (line: string): ParsedNode =
  let splitLine = line.Split(" -> ")
  let name, weight = parseTopNode splitLine.[0]
  let heldNodes = 
    if Array.length splitLine = 1 
      then []
      else parseHolding splitLine.[1] 
  {
    Name = name;
    Weight = weight;
    Holding = heldNodes
  }

let input = File.ReadLines "Day7.txt" |> Seq.map parseLine |> Seq.toList

let flattenHeldNodes = 
  List.fold (fun acc node -> List.append acc node.Holding) []

let isNotContainedBy (list: 'T list) (elem: 'T): bool =
  match List.tryFind ((=) elem) list with
  | Some _ -> false
  | None -> true

let findBottomNode nodeList =
  let listOfHeldNodes = nodeList |> flattenHeldNodes
  nodeList 
  |> List.map (fun node -> node.Name) 
  |> List.find (isNotContainedBy listOfHeldNodes)

let solve nodes =
  printfn "Part 1: %s" (findBottomNode nodes)