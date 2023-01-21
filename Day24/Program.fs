open AoC2022.Day24
open AoC2022.Utilities

// Advent of Code 2022 day 24.
[<EntryPoint>]
let main argv =        
    let elf =
        "InputFiles/Day24Input.txt"  
        |> Seq.ofFileLines
        |> BlizzardValley.create
        |> Elf.create 1000

    let startPos, goalPos = (elf |> Elf.getStartPos), (elf |> Elf.getGoalPos)
    let requiredMinutes1 = elf |> Elf.getShortestPath startPos goalPos 0
    printfn "Part 1 answer is: %d" requiredMinutes1

    let requiredMinutes2 = elf |> Elf.getShortestPath goalPos startPos requiredMinutes1    
    let requiredMinutes3 = elf |> Elf.getShortestPath startPos goalPos requiredMinutes2    
    printfn "Part 2 answer is: %d" requiredMinutes3    
    0