open AoC2022.Utilities

open System

// Advent of Code 2022 day 15.
[<EntryPoint>]
let main argv =   
   
   let sensors =
       "InputFiles/Day15Input.txt"
       |> Seq.ofFileLines
       |> Array.ofSeq
       |> Array.map (fun bl ->
           let tokens = bl.Split ' '
           let sx = tokens.[2].Replace("x=","").Replace(",","") |> int64
           let sy = tokens.[3].Replace("y=","").Replace(":","") |> int64
           let bx = tokens.[8].Replace("x=","").Replace(",","") |> int64
           let by = tokens.[9].Replace("y=","")                 |> int64
           sx,sy,bx,by)
      
   let getInterval targetLine sensors =
       let getInterval' (sx: int64, sy: int64, bx: int64, by: int64) (y: int64) =
           let manhattanDistance = Math.Abs(by-sy) + Math.Abs(bx-sx)
           let yDist = Math.Abs(y-sy)
           match yDist <= manhattanDistance with
           | true  -> Some [|(sx-manhattanDistance+yDist)..(sx+manhattanDistance-yDist)|]
           | false -> None
       sensors
       |> Array.map (fun i -> getInterval' i targetLine)
       |> Array.choose (fun i -> i)
       |> Array.collect (fun i -> i)
       |> Array.distinct
       |> Array.sort

   let getIntervalLength targetLine sensors =       
       let totalLength =
           sensors
           |> getInterval targetLine
           |> Array.length
       let noBeaconsInLine =
           sensors
           |> Array.map (fun (_, _, bx, by) -> bx, by)
           |> Array.distinct
           |> Array.filter (fun (_, by) -> by = targetLine)
           |> Array.length
       totalLength - noBeaconsInLine
    
   printfn "Part 1 answer is: %d" (sensors |> getIntervalLength 2000000L)
  
   let getIntervals targetLine sensors =
       let getInterval' (sx: int64, sy: int64, bx: int64, by: int64) (y: int64) =
           let manhattanDistance = Math.Abs(by-sy) + Math.Abs(bx-sx)
           let yDist = Math.Abs(y-sy)
           match yDist <= manhattanDistance with
           | true  -> Some (sx-manhattanDistance+yDist, sx+manhattanDistance-yDist)
           | false -> None
       sensors
       |> Array.map (fun i -> getInterval' i targetLine)
       |> Array.choose (fun i -> i)
       |> Array.sort

   let findBeacon intervals =
       let areOverlapping interval1 interval2 =
           let i2_1, _ = interval2
           let _, i1_2 = interval1
           i2_1 <= i1_2
       let stack = Stack.empty       
       let intervals =
           (stack, intervals)
           ||> Array.fold (fun s i ->
               match s |> Stack.isEmpty with
               | true  -> s |> Stack.push i
               | false ->   
                   let top, s' = s |> Stack.pop
                   if areOverlapping top i then
                       let i' = if snd i >= snd top then (fst top, snd i) else top
                       s' |> Stack.push i'
                   else
                       s' |> Stack.push top |> Stack.push i)
           |> Stack.toArray
           |> Array.sort
       if intervals.Length = 1 then
           None
       else
           Some (1L+(intervals.[0] |> snd |> int64))
    
   let beaconX, beaconY =
       let y =
           [0L..4000000L]
           |> List.takeWhile (fun y -> (sensors |> getIntervals y) |> findBeacon = None)
           |> List.last
           |> (+) 1L
       let x = sensors |> getIntervals y |> findBeacon |> Option.get
       x,y

   printfn "Part 2 answer is: %d" (4000000L*beaconX + beaconY)
   0
