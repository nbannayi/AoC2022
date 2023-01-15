open AoC2022.Utilities

open AoC2022.Day21

// Advent of Code 2022 day 21.
[<EntryPoint>]
let main argv =
  
   let monkeyYells =
       "InputFiles/Day21Input.txt"
       |> Seq.ofFileLines
      
   let solution1 =
       monkeyYells
       |> MonkeyRiddle.create
       |> MonkeyRiddle.solve

   printfn "Part 1 answer is: %d" (solution1 |> MonkeyRiddle.getRootValue)
  
   let solution2 =
       monkeyYells
       |> MonkeyRiddle.create
       |> MonkeyRiddle.removeHuman  
       |> MonkeyRiddle.solve
      |> MonkeyRiddle.getHumanYell
         
   printfn "Part 2 answer is: %d" solution2
   0