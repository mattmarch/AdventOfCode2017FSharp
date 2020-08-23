open System

let solveDay day = 
  match day with
    | "1" -> Day1.solve Day1.input
    | "2" -> Day2.solve Day2.input
    | "3" -> Day3.solve Day3.input
    | "4" -> Day4.solve Day4.input
    | _ -> printfn "Input doesn't seem to match any days"


[<EntryPoint>]
let main argv =
  solveDay argv.[0]
  0 // return an integer exit code
 