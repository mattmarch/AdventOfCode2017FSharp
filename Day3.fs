module Day3

let input = 277678

type Position = {X: int; Y: int}

let manhattanDistance position = abs(position.X) + abs(position.Y)

type Direction = Up | Down | Left | Right

let moveInDirection direction {X = x; Y = y} =
  match direction with
    | Up -> {X = x; Y = y + 1}
    | Down -> {X = x; Y = y - 1}
    | Left -> {X = x - 1; Y = y}
    | Right -> {X = x + 1; Y = y}

let getNextDirection direction =
  match direction with
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

type Limits = {
  Top: int
  Bottom: int
  Left: int
  Right: int
}

type State = {
  Position: Position
  Direction: Direction
  Limits: Limits
}

let increaseLimit direction limit = 
  match direction with
    | Up -> {limit with Top = limit.Top + 1}
    | Down -> {limit with Bottom = limit.Bottom - 1}
    | Left -> {limit with Left = limit.Left - 1}
    | Right -> {limit with Right = limit.Right + 1}

let exceedsLimits {Top = top; Bottom = bottom; Left = left; Right = right} {X = x; Y = y} =
  x > right || x < left || y < bottom || y > top


let getNextState {Position = position; Direction = direction; Limits = limits;} =
  let nextPosition = position |> moveInDirection direction
  match nextPosition |> exceedsLimits limits with
    | false -> {Position = nextPosition; Direction = direction; Limits = limits;}
    | true -> {Position = nextPosition; Direction = getNextDirection direction; Limits = increaseLimit direction limits}

let initialState = {
  Position = {X = 0; Y = 0};
  Direction = Right;
  Limits = {Top = 0; Bottom = 0; Left = 0; Right = 0}
}

let stateIterator state = 
  let nextState = getNextState state
  Some (nextState.Position, nextState)

let gridPositions = Seq.append (Seq.singleton {X = 0; Y = 0}) (Seq.unfold stateIterator initialState)

let takeIndex index sequence = sequence |> Seq.take index |> Seq.last

// Part 2

let getSurroundingCells position =
  let possibleCells = seq {-1; 0; 1}
  Seq.allPairs possibleCells possibleCells 
    |> Seq.filter (fun (x, y) -> not (x = 0 && y = 0))
    |> Seq.map (fun (x, y) -> {X = position.X + x; Y = position.Y + y})

let initialSumValues = Map.empty.Add({X = 0; Y = 0}, 1)

let rec findGreaterThan threshold (sumValues: Map<Position, int>) currentState =
  let nextState = getNextState currentState
  let cellValue = getSurroundingCells nextState.Position
                    |> Seq.choose (fun position -> Map.tryFind position sumValues)
                    |> Seq.sum
  match cellValue > threshold with
    | true -> cellValue
    | false -> findGreaterThan threshold (sumValues.Add(nextState.Position, cellValue)) nextState


let solve (inputNumber: int) = 
  printfn "Part 1: %i" (takeIndex inputNumber gridPositions |> manhattanDistance)
  printfn "Part 2: %i" (findGreaterThan inputNumber initialSumValues initialState)