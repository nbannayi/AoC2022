open AoC2022.Utilities

// Advent of Code 2022 day 4.
[<EntryPoint>]
let main argv =
    
    let pairRanges =
        "InputFiles/Day04Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (fun p -> 
            let tokens = p.Split ','
            let tokens' = tokens |> Array.map (fun t -> t.Split '-')
            [|tokens'.[0].[0] |> int; tokens'.[0].[1] |> int; tokens'.[1].[0] |> int; tokens'.[1].[1] |> int|])
        
    let isFullyContained (ranges: int array) =
        match ranges.[0], ranges.[2] with
        | r1, r2 when r1 < r2 -> ranges.[1] >= ranges.[3] // Second pair fully contained in first.
        | r1, r2 when r1 > r2 -> ranges.[1] <= ranges.[3] // First pair fully contained in second.
        | _ -> true                                       // First pair always fully contained in second or vice-versa.
            
    let isOverlapping (ranges: int array) =
        let rangeSet1 = [ranges.[0]..ranges.[1]] |> Set.ofList
        let rangeSet2 = [ranges.[2]..ranges.[3]] |> Set.ofList
        rangeSet1 |> Set.intersect rangeSet2 |> Set.count |> (<>) 0

    let noFullyContained =
        pairRanges
        |> Seq.filter (fun pr -> pr |> isFullyContained)
        |> Seq.length
    
    printfn "Part 1 answer is: %d" noFullyContained

    let noOverlapping =
        pairRanges
        |> Seq.filter (fun pr -> pr |> isOverlapping)
        |> Seq.length

    printfn "Part 2 answer is: %d" noOverlapping
    0