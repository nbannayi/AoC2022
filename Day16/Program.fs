open AoC2022.Utilities
open AoC2022.Day16

// Advent of Code 2022 day 16.
[<EntryPoint>]
let main argv =
    
    let valveSystem =
        "InputFiles/Day16Input.txt"
        |> Seq.ofFileLines
        |> ValveSystem.create

    valveSystem |> ValveSystem.traverse ["AA", 0] 0 30
    let maxPressure = valveSystem.Paths.Values |> Seq.max
    printfn "Part 1 answer is: %d" maxPressure

    let valveSystem' =
        "InputFiles/Day16Input.txt"
        |> Seq.ofFileLines
        |> ValveSystem.create

    valveSystem' |> ValveSystem.traverse ["AA", 0] 0 26
    let maxPressure' = 
        seq {
            for a in valveSystem'.Paths.Keys do                
                for b in valveSystem'.Paths.Keys do
                    if a &&& b = 0 then // < this is to get inverse path e.g. 1101 vs 0010.
                        valveSystem'.Paths.[a] + valveSystem'.Paths.[b]
        } |> Seq.max
    printfn "Part 2 answer is: %d" maxPressure'
    0 
