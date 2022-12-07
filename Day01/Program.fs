open AoC2022.Utilities

[<EntryPoint>]
let main argv =
    let caloriesList = 
        "InputFiles/Day01Input.txt" 
        |> Seq.ofFileChunks "\n\n"
        |> Seq.map (fun c -> c.Split([|"\n"|], System.StringSplitOptions.None) |> Array.map (int))

    let combinedCalories =
        caloriesList
        |> Seq.map (fun c -> c |> Array.sum)

    printfn "Part 1 answer is: %d" (combinedCalories |> Seq.max)

    let sumOftopThreeCalories =
        combinedCalories
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.sum 
        
    printfn "Part 2 answer is: %d" sumOftopThreeCalories
    0