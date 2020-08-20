module Day1

let inputString = System.IO.File.ReadAllText("Day1.txt")

let pairAreEqual (a, b) = a = b

let solve = 
  let input = inputString |> Seq.map (fun x -> int x - int '0')
  let offsetInput = Seq.head input |> Seq.singleton |> Seq.append (Seq.tail input)
  let zippedInput = Seq.zip input offsetInput
  zippedInput |> Seq.filter pairAreEqual |> Seq.sumBy (fun (a, _) -> a)
