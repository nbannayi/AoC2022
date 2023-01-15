open AoC2022.Day20
open AoC2022.Utilities
 
// Advent of Code 2022 day 20.
[<EntryPoint>]
let main argv =
 
    let sumOfGroveCoords1 =
        "InputFiles/Day20Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (int)
        |> EncryptionFile.create 1L
        |> EncryptionFile.mix 1
        |> EncryptionFile.getGroveCoordinates
        |> List.sum
   
    printfn "Part 1 answer is: %d" sumOfGroveCoords1
 
    let sumOfGroveCoords2 =
        "InputFiles/Day20Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (int)
        |> EncryptionFile.create 811589153L
        |> EncryptionFile.mix 10
        |> EncryptionFile.getGroveCoordinates
        |> List.sum
 
    printfn "Part 2 answer is: %d" sumOfGroveCoords2
    0
 