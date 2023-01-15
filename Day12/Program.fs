open AoC2022.Utilities

open AoC2022.Day12
 
// Advent of Code 2022 day 12.
[<EntryPoint>]
let main argv =

    let climber =
        "InputFiles/Day12Input.txt"
        |> Seq.ofFileLines
        |> Array.ofSeq
        |> Grid.create
        |> HillClimber.create       

    let shortestPath =
        climber
        |> HillClimber.climb
        |> HillClimber.getShortestPath

    printfn "Part 1 answer is: %d" (shortestPath |> Seq.length)
 
    let lowestPoints =
        climber
        |> HillClimber.getAllLowestPoints
 
    let shortestScenicPath =
        lowestPoints
        |> List.mapi (fun i lp ->
            climber
            |> HillClimber.reset
            |> HillClimber.setStartPosition lp
            |> HillClimber.climb
            |> HillClimber.getShortestPath
            |> Seq.length)
        |> List.filter (fun sp -> sp <> 0)
        |> List.min

    printfn "Part 2 answer is: %A" shortestScenicPath
    0