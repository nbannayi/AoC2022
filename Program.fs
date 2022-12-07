open AoC2022.Utilities

// Advent of Code 2022 day 6.
[<EntryPoint>]
let main argv =
    
    let signal = "InputFiles/Day06Input.txt" |> Seq.ofFileLines |> Seq.head
    
    let getStartPosition n signal =        
        signal
        |> Seq.windowed n
        |> Seq.findIndex (fun s -> s |> Seq.distinct |> Seq.length |> (=) n)
        |> (+) n

    printfn "Part 1 answer is: %d" (signal |> getStartPosition 4)
    printfn "Part 2 answer is: %d" (signal |> getStartPosition 14)
    0