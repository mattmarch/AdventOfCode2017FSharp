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

type Node = {
  Name: Name;
  Weight: int;
  Holding: Node list;
}

let rec createNodeTree baseNode (parsedNodes: ParsedNode list) =
  let node = parsedNodes |> List.find (fun n -> n.Name = baseNode)
  {
    Name = node.Name;
    Weight = node.Weight;
    Holding = node.Holding |> List.map (fun n -> createNodeTree n parsedNodes)
  }

type BalanceCheckResult =
  | BalancedWeight of int
  | Unbalanced of int list

let getUnbalanced results =
  let unbalancedInResults = results |> List.tryPick (fun r ->
    match r with
    | BalancedWeight _ -> None
    | Unbalanced weights -> Some (Unbalanced weights)
    )
  match unbalancedInResults with
  | Some unbalancedResult -> Some unbalancedResult
  | None -> match (results |> List.distinct |> List.length > 1) with
            | true -> Some (Unbalanced (results |> List.choose (fun r -> match r with
                                                                          | BalancedWeight w -> Some w
                                                                          | _ -> None
                                                                          )))
            | false -> None

let sumBalancedWeights =
  List.choose (fun r -> match r with
                        | BalancedWeight w -> Some(w)
                        | _ -> None
  ) >> List.sum

let rec checkBalance node =
  let heldResults = node.Holding |> List.map checkBalance
  match getUnbalanced heldResults with
  | Some unbalanced -> unbalanced
  | None -> BalancedWeight ((heldResults |> sumBalancedWeights) + node.Weight)


let solve nodes =
  let bottomNode = findBottomNode nodes
  printfn "Part 1: %s" bottomNode
  printfn "Part 2: %A" (nodes |> (createNodeTree bottomNode) |> checkBalance)