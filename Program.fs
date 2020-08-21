open System

let solveDay day = 
  match day with
    | "1" -> Day1.solve Day1.input
    | _ -> printfn "Input doesn't seem to match any days"


[<EntryPoint>]
let main argv =
  solveDay argv.[0]
  0 // return an integer exit code
 