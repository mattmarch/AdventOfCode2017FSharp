module Day8

open System.IO

type Operation = Increment | Decrement

type Register = string

type Registers = Map<Register, int>

type Comparator =
  | GreaterThan
  | LessThan
  | EqualTo
  | NotEqualTo
  | GreaterThanOrEqualTo
  | LessThanOrEqualTo

type Condition = {
  Register: string;
  Comparator: Comparator;
  Value: int;
}

type Instruction = {
  Register: Register;
  Operation: Operation;
  Amount: int;
  Condition: Condition;
}

let parseOperator operatorString =
  match operatorString with
  | "inc" -> Increment
  | "dec" -> Decrement
  | _ -> failwithf "Invalid operator %s" operatorString

let parseComparator comparatorString =
  match comparatorString with
  | ">" -> GreaterThan
  | "<" -> LessThan
  | "==" -> EqualTo
  | "!=" -> NotEqualTo
  | ">=" -> GreaterThanOrEqualTo
  | "<=" -> LessThanOrEqualTo
  | _ -> failwithf "Invalid comparator %s" comparatorString

let parseLine (line: string) =
  let lineParts = line.Split(" ")
  {
    Register = lineParts.[0]
    Operation = lineParts.[1] |> parseOperator;
    Amount = lineParts.[2] |> int;
    Condition = {
      Register = lineParts.[4]
      Comparator = lineParts.[5] |> parseComparator
      Value = lineParts.[6] |> int;
    }
  }

let input = File.ReadLines "Day8.txt" |> Seq.map parseLine

let getRegisterValue (registers: Registers) register =
  match registers.TryFind register with
  | Some value -> value
  | None -> 0

let evaluateCondition registers (condition: Condition) =
  let registerValue = getRegisterValue registers condition.Register
  match condition.Comparator with
  | GreaterThan -> registerValue > condition.Value
  | LessThan -> registerValue < condition.Value
  | EqualTo -> registerValue = condition.Value
  | NotEqualTo -> registerValue <> condition.Value
  | GreaterThanOrEqualTo -> registerValue >= condition.Value
  | LessThanOrEqualTo -> registerValue <= condition.Value

let applyOperation registers register operation amount =
  let currentRegisterValue = getRegisterValue registers register
  match operation with
  | Increment -> registers.Add(register, currentRegisterValue + amount)
  | Decrement -> registers.Add(register, currentRegisterValue - amount)

type State = {
  Registers: Registers;
  CurrentMaximum: int;
}

let applyInstruction state instruction =
  if evaluateCondition state.Registers instruction.Condition then 
    let nextRegistersState = applyOperation state.Registers instruction.Register instruction.Operation instruction.Amount
    let newRegisterValue = getRegisterValue nextRegistersState instruction.Register
    {
      Registers = nextRegistersState;
      CurrentMaximum = 
        if newRegisterValue > state.CurrentMaximum then newRegisterValue else state.CurrentMaximum;
    }
  else state

let applyInstructions: Instruction seq -> State =
  Seq.fold applyInstruction ({Registers = Registers []; CurrentMaximum = 0})

let solve instructions =
  let endState = applyInstructions instructions
  printfn "Part 1: %i" (endState.Registers
    |> Map.toList 
    |> List.maxBy snd
    |> snd
  )
  printfn "Part 2: %i" endState.CurrentMaximum
    