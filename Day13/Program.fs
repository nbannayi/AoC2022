open AoC2022.Utilities
open AoC2022.Day13
 
// Advent of Code 2022 day 13.
[<EntryPoint>]
let main argv =
     
    let signalPairs =
        "InputFiles/Day13Input.txt"
        |> Seq.ofFileChunks "\n\n"
        |> Seq.map (fun s -> s.Split([|"\n"|], System.StringSplitOptions.None))
        |> Seq.map (fun s -> s.[0] |> Signal.parseSignal, s.[1] |> Signal.parseSignal)
        |> Array.ofSeq
 
    let sumCorrectSignals =
        signalPairs
        |> Array.mapi (fun i sp -> i+1, sp ||> Signal.compare)
        |> Array.filter (fun (_, ss) -> ss = LeftRight)
        |> Array.sumBy (fst)
 
    printfn "Part 1 answer is: %d" sumCorrectSignals
 
    let divider2, divider6 =
        "[[2]]" |> Signal.parseSignal, "[[6]]" |> Signal.parseSignal
 
    let signals =
        signalPairs
        |> Array.map (fun (sp1, sp2) -> [|sp1;sp2|])
        |> Array.collect (fun sp -> sp)
        |> Array.append [|divider2; divider6|]
        |> Signal.sort
 
    let decoderKey =
        ((signals |> Array.findIndex (fun s -> s = divider2)) + 1) *
        ((signals |> Array.findIndex (fun s -> s = divider6)) + 1)
   
    printfn "Part 2 answer is: %d" decoderKey
    0