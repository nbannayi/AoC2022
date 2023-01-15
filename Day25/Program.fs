open AoC2022.Utilities
 
open System
 
// Advent of Code 2022 day 25.
[<EntryPoint>]
let main argv =
 
    let snafuNumbers =
        "InputFiles/Day25Input.txt"
        |> Seq.ofFileLines
 
    let snafuToDec n =
        (n |> Seq.rev)
        |> Seq.mapi (fun i c -> 5.**(float i) * match c with '-' -> -1. | '=' -> -2. | _ -> c |> Char.toInt |> float) 
        |> Seq.sum
        |> int64
   
    let decToSnafu n =
        let rec decToSnafu' n =
            seq {
                let (divdm, moddn) = n / 5L, n % 5L
                let sn, divdm' = match moddn with 4L -> "-", divdm+1L | 3L -> "=", divdm+1L | _ -> string moddn, divdm
                yield sn
                if divdm' <> 0L then yield! decToSnafu' divdm'
            }
        (n |> decToSnafu' |> Seq.rev)
        |> String.concat ""
 
    let snafuNumber =
        snafuNumbers
        |> Seq.map (snafuToDec)
        |> Seq.sum
        |> decToSnafu
 
    printfn "Part 1 answer is: %s" snafuNumber   
    0