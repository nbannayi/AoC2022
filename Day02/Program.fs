open AoC2022.Utilities

// Advent of Code 2022 day 2.
[<EntryPoint>]
let main argv =
    
    let strategyList =         
        "InputFiles/Day02Input.txt" 
        |> Seq.ofFileLines
        |> Seq.map (fun sl -> let tokens = sl.Split ' ' in tokens.[0],tokens.[1])

    let getItemScore item =
        match item with
        | "A" | "X" -> 1 | "B" | "Y" -> 2 | "C" | "Z" -> 3
        | _ -> failwith "Unsupported item."

    let getGameScore item1 item2 =
        match item1, item2 with
        | "A","X" | "B","Y" | "C","Z" -> 3 | "A","Y" | "B","Z" | "C","X" -> 6 | "A","Z" | "B","X" | "C","Y" -> 0
        | _ -> failwith "Invalid game items."
        
    let executeStrategy item1 item2 =
        match item1, item2 with
        | "A","X" -> "A","Z" | "B","X" -> "B","X" | "C","X" -> "C","Y" // Lose.
        | "A","Y" -> "A","X" | "B","Y" -> "B","Y" | "C","Y" -> "C","Z" // Draw.
        | "A","Z" -> "A","Y" | "B","Z" -> "B","Z" | "C","Z" -> "C","X" // Win.
        | _ -> failwith "Invalid game items."
        
    let strategyScore1 =
        strategyList
        |> Seq.map (fun (i1,i2) -> (getItemScore i2) + getGameScore i1 i2)
        |> Seq.sum

    printfn "Part 1 answer is: %d" strategyScore1

    let strategyScore2 =
        strategyList
        |> Seq.map (fun (i1,i2) -> executeStrategy i1 i2)
        |> Seq.map (fun (i1,i2) -> (getItemScore i2) + getGameScore i1 i2)
        |> Seq.sum

    printfn "Part 2 answer is: %d" strategyScore2
    0