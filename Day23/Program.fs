open AoC2022.Day23
open AoC2022.Utilities

// Advent of Code 2022 day 23.
// Note this code is not very optimal, part 2 takes around 2 mins to run.
[<EntryPoint>]
let main argv =
    let field =
        "InputFiles/Day23Input.txt"  
        |> Seq.ofFileLines
        |> Array.ofSeq

    let emptyGroundCount =
        Grove.create field
        |> Grove.moveElvesNRounds 10
        |> Grove.getEmptyGroundCount
    
    printfn "Part 1 answer is: %d" emptyGroundCount

    let requiredRounds =
        Grove.create field
        |> Grove.moveElvesAllRounds
        |> fun g -> g.Round+1 // Add 1 for round where no elves move.

    printfn "Part 2 answer is: %d" requiredRounds
    0
