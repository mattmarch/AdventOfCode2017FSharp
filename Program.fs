open System

let matchDay day = 
  match day with
    | "1" -> printfn "%A" Day1.solve
    | _ -> printfn "Input doesn't seem to match any days"


[<EntryPoint>]
let main argv =
  matchDay argv.[0]
  0 // return an integer exit code
 