open AoC2022.Utilities
open AoC2022.Day17
 
open System
 
// Advent of Code 2022 day 17.
// Note this code is pretty slow and could use optimising, takes around 2-3 mins to run.
[<EntryPoint>]
let main argv =
   
    let movementCycle =
        "InputFiles/Day17Input.txt"
        |> Seq.ofFileLines
        |> Seq.head
        |> Seq.map (fun d -> match d with '<' -> Left | '>' -> Right | _ -> failwith "Unsupported direction")
        |> Array.ofSeq
       
    let chamber =
        TetrisChamber.create HorizLine movementCycle 
        |> TetrisChamber.fillToNoBricks 2022L
               
    printfn "Part 1 answer is: %d" (chamber |> TetrisChamber.getLevel)   
           
    // Widen search slightly for part 2 (found out by experimentation.)
    let chamber' =
        TetrisChamber.create HorizLine movementCycle 
        |> TetrisChamber.fillToLevel 3000L
               
    // Now search from start for first window that repeats.
    // (This code is not very efficient.)
    let findCycleLevels chamber =       
        // Turn 2D array into a 1D array of strings.
        let chamberOutput =    
            [|for y in [0..chamber.Height-1] do           
                ([for x in [1..7] -> string chamber.Grid.[x,y]] |> String.concat "")|]
            |> Array.filter (fun r -> r |> Seq.contains '#')
            |> Array.rev               
        let outputCycles =               
            chamberOutput
            // Turn into 1 long string.
            |> String.concat ""
            // Turn into an array of 20 row windows.
            |> Seq.windowed 140 // <- 10 rows wasn't enough but 20 worked for me but may not for all inputs.
            |> Array.ofSeq
        let rec findCycle i (cycles: char array array) =                       
            let cycle = cycles.[i]
            let found =
                cycles
                |> Array.mapi (fun i c -> i, c)
                |> Array.filter (fun (_,c) -> c = cycle)
                |> Array.map (fun (i,_) -> i)
            match found.Length with
            | 0 | 1 -> findCycle (i+1) cycles
            | _ -> found
        let cycles' = findCycle 0 outputCycles
        let length = chamberOutput.Length
        length-cycles'.[0]/7 |> int64, length-cycles'.[1]/7 |> int64
   
    // ------ Part 2 workings ------
    // Sketch of solution using example:
    // No. bricks: 16, Level: 26
    // No. bricks: 51, Level: 79
    // No. bricks: 86, Level: 132
    //------------------------------
    // 26 levels at 16 bricks
    // 2022 - 16 = 2006
    // 2006 / 35 = 57, 57 * 53 levels = 3021, 3021 + 26 = 3047 so far.
    // 11 bricks left, fill to 16 + 11 = 27 then subtract 26 from result.
    // 47 - 26 = 21, 3047 + 21 = 3,068 !
    //------------------------------
   
    // Implementation of above ^
    let targetNoBricks = 1000000000000L
    let initialCycleLevel, secondCycleLevel = chamber' |> findCycleLevels
    let getNoCycleBricks cycleLevel =
        TetrisChamber.create HorizLine movementCycle 
        |> TetrisChamber.fillToLevel cycleLevel
        |> fun tc -> tc.NoBricks   
    let initialNoCycleBricks = getNoCycleBricks initialCycleLevel
    let noCycleBricks = getNoCycleBricks secondCycleLevel - initialNoCycleBricks
    let remainingNoBricks = targetNoBricks - initialNoCycleBricks
    let completeCycleLevel = initialCycleLevel + (remainingNoBricks / noCycleBricks) * (secondCycleLevel - initialCycleLevel)
    let finalNoCycleBricks = remainingNoBricks % noCycleBricks
    let finalCycleLevel =
        TetrisChamber.create HorizLine movementCycle
        |> TetrisChamber.fillToNoBricks (initialNoCycleBricks + finalNoCycleBricks)
        |> fun tc -> (tc |> TetrisChamber.getLevel) - initialCycleLevel
    let level' = completeCycleLevel + finalCycleLevel
   
    printfn "Part 2 answer is: %d" level'       
    0