namespace AoC2022.Day24 

open AoC2022.Utilities

type Blizzard =
    {
        Position:  Coordinate
        Direction: Direction
    }

type BlizzardValley =
    {
        Grid:      char [,]
        Minutes:   int
        Blizzards: Blizzard list        
    }

module BlizzardValley =

    /// Create a new blizzard valley.
    let create (valley: string seq) =
        let valley = valley |> Array.ofSeq
        let grid =
            let contents = Array2D.init valley.[0].Length valley.Length (fun _ _ -> '.')
            for y in [0..valley.Length-1] do
                valley.[y] |> Seq.iteri (fun i x -> contents.[i,y] <- x)
            contents
        let blizzards =
            [for y in [0..(grid |> Array2D.length2)-1] do
                for x in [0..(grid |> Array2D.length1)-1] ->
                    let position = {X = x; Y = y}
                    let direction =
                        match grid.[x,y] with
                        | '>' -> Some Right
                        | '<' -> Some Left
                        | 'v' -> Some Down
                        | '^' -> Some Up
                        | _   -> None
                    match direction with
                    | Some d -> Some {Position = position; Direction = d}
                    | None   -> None]
            |> List.choose (fun b -> b)                
        {
            Grid      = grid
            Minutes   = 0
            Blizzards = blizzards
        }

    /// Display the passed blizzard valley.
    let display (valley: BlizzardValley) =        
        for y in [0..(valley.Grid |> Array2D.length2)-1] do
            for x in [0..(valley.Grid |> Array2D.length1)-1] do
                printf "%c" valley.Grid.[x,y]
            printfn ""

    /// Update the blizzard valley in 1 minute.
    let update (valley: BlizzardValley) =
        let grid = valley.Grid |> Array2D.copy        
        for y in [0..(grid |> Array2D.length2)-1] do
            for x in [0..(grid |> Array2D.length1)-1] do
                let c = grid.[x,y]
                grid.[x,y] <- match c with | '#' -> c | _ -> '.'
        // Move blizzards.
        let blizzards' =
            let rightEdge = grid |> Array2D.length1
            let lowerEdge = grid |> Array2D.length2
            valley.Blizzards
            |> List.map (fun b ->
                let position' = 
                    match b.Direction with
                    | Up    -> {X = b.Position.X;   Y = b.Position.Y-1} 
                    | Down  -> {X = b.Position.X;   Y = b.Position.Y+1}
                    | Left  -> {X = b.Position.X-1; Y = b.Position.Y}
                    | Right -> {X = b.Position.X+1; Y = b.Position.Y}
                    | _     -> failwith "Invalid direction." 
                let x', y' =
                    match position'.X, position'.Y with
                    | x, y when x < 1           -> rightEdge-2, y
                    | x, y when x > rightEdge-2 -> 1, y
                    | x, y when y < 1           -> x, lowerEdge-2
                    | x, y when y > lowerEdge-2 -> x, 1
                    | x, y -> x, y
                {b with Position = {X = x'; Y = y'}})
        // Update valley.
        let processBlizzard curBlizzard newBlizzard =
            match curBlizzard, newBlizzard with
            | '2', _ -> '3'
            | '3', _ -> '4'
            | cb,  _ when ['>'; '<'; '^'; 'v'] |> List.contains cb -> '2' 
            | _ -> newBlizzard
        blizzards'
        |> List.iter (fun b ->
            let curBlizzard = grid.[b.Position.X,b.Position.Y]
            grid.[b.Position.X,b.Position.Y] <-
                match b.Direction with
                | Left  -> processBlizzard curBlizzard '<'
                | Right -> processBlizzard curBlizzard '>'
                | Up    -> processBlizzard curBlizzard '^'
                | Down  -> processBlizzard curBlizzard 'v'
                | _     -> failwith "Invalid direction.")
        {valley with Grid = grid; Blizzards = blizzards'; Minutes = valley.Minutes+1}

    /// Get start position (gap in top wall.)
    let getStartPos (valley: BlizzardValley) =            
        let x = [0..(valley.Grid |> Array2D.length1)] |> List.find (fun i -> valley.Grid.[i,0] = '.')
        {X = x; Y = 0}

    /// Get goal position (gap on bottom wall.)
    let getGoalPos (valley: BlizzardValley) =
        let x1 = (valley.Grid |> Array2D.length1)-1
        let x2 = (valley.Grid |> Array2D.length2)-1
        let x = [0..x1] |> List.find (fun i -> valley.Grid.[i,x2] = '.')        
        {X = x; Y = x2}