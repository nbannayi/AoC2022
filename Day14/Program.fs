open AoC2022.Utilities
open AoC2022.Day14
 
// Advent of Code 2022 day 14.
[<EntryPoint>]
let main argv =
   
    let paths =       
        "InputFiles/Day14Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (fun p ->
            p.Split([|" -> "|], System.StringSplitOptions.None)
            |> Array.map (fun p ->
                let tokens = p.Split ','
                tokens.[0] |> int, tokens.[1] |> int))
        |> Array.ofSeq
                               
    let sandGrid1 =
        paths
        |> SandGrid.create
        |> SandGrid.plotPaths paths
        |> SandGrid.plotSandSource
        |> SandGrid.moveSand
           
    printfn "Part 1 answer is: %d" (sandGrid1 |> SandGrid.countSand)
 
    let paths' =
        paths
        // Add "infinity" line
        |> Array.append [| [|(sandGrid1.XOffset-300,sandGrid1.MaxY+2); (sandGrid1.MaxX+300, sandGrid1.MaxY+2)|] |]
 
    let sandGrid2 =
        paths'        
        |> SandGrid.create       
        |> SandGrid.plotPaths paths'
        |> SandGrid.plotSandSource
        |> SandGrid.moveSand
 
    printfn "Part 2 answer is: %d" (sandGrid2 |> SandGrid.countSand)   
    0