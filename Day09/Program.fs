open AoC2022.Utilities 
open AoC2022.Day09 
 
// Advent of Code 2022 day 9. 
[<EntryPoint>] 
let main argv = 
 
    let motions = 
        "InputFiles/Day09Input.txt"  
        |> Seq.ofFileLines 
        |> Seq.map (fun l ->  
            let tokens = l.Split ' ' in tokens.[0] |> Direction.parse, tokens.[1] |> int) 
                 
    let rope = (Rope.create 1, motions) ||> Seq.fold (fun r m -> r |> Rope.move m)     
    printfn "Part 1 answer is: %d" (rope |> Rope.noTailVisited) 
 
    let rope' = (Rope.create 9, motions) ||> Seq.fold (fun r m -> r |> Rope.move m)     
    printfn "Part 2 answer is: %d" (rope' |> Rope.noTailVisited) 
    0 