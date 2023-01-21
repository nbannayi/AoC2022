namespace AoC2022.Day24 

open AoC2022.Utilities
open System.Collections.Generic

type Elf =
    {
        Valleys:  BlizzardValley array
        Position: Coordinate        
    }

module Elf =

    /// Create an elf with a memory of n blizzard valley configurations.
    let create n valley =        
        let valleys = [|0..n-1|] |> Array.scan (fun v _ -> v |> BlizzardValley.update) valley
        let position = valleys.[0] |> BlizzardValley.getStartPos
        {Valleys=valleys; Position=position}

    /// Get start position for the elf.
    let getStartPos (elf: Elf) =
        elf.Valleys.[0] |> BlizzardValley.getStartPos

    /// Get goal position for the elf.
    let getGoalPos (elf: Elf) =
        elf.Valleys.[0] |> BlizzardValley.getGoalPos

    /// Get valley at given minute.
    let getValley minute elf =
        elf.Valleys.[minute]

    /// Get moves elf can make from a given position at a given minute.
    let getMoves (elfPos: Coordinate) minute (elf: Elf) =        
        let up     = {X=elfPos.X;   Y=elfPos.Y-1}
        let down   = {X=elfPos.X;   Y=elfPos.Y+1}
        let left   = {X=elfPos.X-1; Y=elfPos.Y}
        let right  = {X=elfPos.X+1; Y=elfPos.Y}
        let valley = elf.Valleys.[minute]
        let rightBound, lowerBound = valley.Grid |> Array2D.length1, valley.Grid |> Array2D.length2
        let canMove (position: Coordinate) =
            let outOfBounds (position: Coordinate) =
                position.X < 0 || position.Y < 0 || position.X > rightBound-1 || position.Y > lowerBound-1        
            match (outOfBounds position) with
            | true  -> false
            | false -> valley.Grid.[position.X,position.Y] = '.'
        [right;down;up;left;elfPos]
        |> List.filter (fun m -> m |> canMove)

    /// Get shortest path from startPos to goalPos at given starting minute.
    let getShortestPath startPos endPos minute elf =
        let queue   = Queue<Coordinate * int>([startPos,minute])
        let visited = HashSet<Coordinate * int>() // < this is important as removes dupes.
        seq {
            while queue.Count > 0 do
                let position, minute = queue.Dequeue()
                match position = endPos with
                | true  ->                
                    queue.Clear()
                    yield minute
                | false ->                                            
                    if not (visited.Contains(position,minute)) then
                        visited.Add(position,minute) |> ignore
                        elf
                        |> getMoves position (minute+1)
                        |> List.iter (fun m -> queue.Enqueue(m,minute+1))
        } |> Seq.head