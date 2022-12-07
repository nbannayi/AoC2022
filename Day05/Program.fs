open AoC2022.Utilities
open AoC2022.Utilities.Stack
open System

// Advent of Code 2022 day 5.
[<EntryPoint>]
let main argv =
    
    let rawInput =
        "InputFiles/Day05Input.txt"
        |> Seq.ofFileChunks "\n\n" 
        |> Array.ofSeq

    let rearrangements =
        rawInput.[1].Split([|"\n"|], StringSplitOptions.None)
        |> Array.map (fun l -> let tokens = l.Split ' ' in [|tokens.[1] |> int; tokens.[3] |> int; tokens.[5] |> int|])        

    let stacks () =        
        let getCrates (line: string) = [|1..4..33|] |> Array.map (fun p -> Some line.[p])          
        rawInput.[0].Split([|"\n"|], StringSplitOptions.None)
        |> Array.map (getCrates)                                                                          // Parse raw input into array.
        |> Array.transpose                                                                                // Transpose to get the same shape.
        |> Array.map (fun s -> s |> Array.filter (fun c -> c <> Some ' ' && not (Char.IsDigit(c.Value)))) // Removes empty slots and stack number.
        |> Array.map (fun s -> Stack.StackContents (s |> List.ofArray))                                   // Convert to array of stacks.
   
    let rearrange1 noStacks fromStack toStack (stacks: Stack<char option> array) =
        [1..noStacks] |> List.iter (fun _ ->        
            let topCrate, newFromStack = stacks.[fromStack-1] |> Stack.pop
            let newToStack = stacks.[toStack-1] |> Stack.push topCrate
            stacks.[fromStack-1] <- newFromStack
            stacks.[toStack-1]   <- newToStack)
        stacks    
                      
    let getFinalRearrangement rearragementFunction stacks (rearrangements: int [] array) = 
        (stacks, rearrangements) 
        ||> Array.fold (fun st arr -> st |> rearragementFunction arr.[0] arr.[1] arr.[2])
        |> Array.map (fun s -> s |> Stack.peek |> Option.get)
        |> Seq.charSeqToString
    
    printfn "Part 1 answer is: %s" (getFinalRearrangement rearrange1 (stacks ()) rearrangements)
    
    let rearrange2 noStacks fromStack toStack (stacks: Stack<char option> array) =              
        // Reverse first n crates.
        let fromStackArray = stacks.[fromStack-1] |> Stack.toArray
        let fromStackSubarray = fromStackArray |> Array.take noStacks |> Array.rev
        fromStackSubarray |> Array.iteri (fun i c -> fromStackArray.[i] <- c)        
        stacks.[fromStack-1] <- Stack.StackContents (fromStackArray |> List.ofArray)
        // Now call earlier rearrangement.
        stacks |> rearrange1 noStacks fromStack toStack
    
    printfn "Part 2 answer is: %s" (getFinalRearrangement rearrange2 (stacks ()) rearrangements)
    0