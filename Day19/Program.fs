open AoC2022.Utilities
open AoC2022.Day19
open System.Diagnostics

// Advent of Code 2022 day 19.

// Note this solution is very slow, I need to spend more time trying to optimise at some point.
// Part 1 was ok, but part 2 took around 9-10 minutes on my old Mac - I think there is an optimisation I'm missing!!
// To be honest I'm just relieved to have finished 2022 which I found quite tricky in places.

// Days 22, 16 and 19 were easily the hardest for me in that order...days 14 and 17 were my favourites. 
[<EntryPoint>]
let main argv =

    let blueprints =
        "InputFiles/Day19Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (Blueprint.create)
        |> Array.ofSeq

    let sw = new Stopwatch()
    sw.Start()

    let qualityScore =
        blueprints
        |> Array.map (fun bp -> RobotState.create bp)
        |> Array.map (fun rs ->
            let maxGeodes = rs |> RobotState.getMaxGeodes 24
            let quality = rs.Blueprint.Id * maxGeodes
            printfn "Blueprint %d: max geodes is %d, quality %d" rs.Blueprint.Id maxGeodes quality
            quality)
        |> Array.sum
        
    sw |> Time.displayElapsed

    printfn "Part 1 answer is: %d" qualityScore
    printfn ""
    
    sw.Reset()
    sw.Start()    

    let qualityScore' =
        blueprints
        |> Array.take 3
        |> Array.map (fun bp -> RobotState.create bp)
        |> Array.map (fun rs ->
            let maxGeodes = rs |> RobotState.getMaxGeodes 32
            printfn "Blueprint %d: max geodes is %d" rs.Blueprint.Id maxGeodes
            maxGeodes)
        |> Array.reduce (*)

    sw |> Time.displayElapsed

    printfn "Part 2 answer is: %d" qualityScore'    
    0