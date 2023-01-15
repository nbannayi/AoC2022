namespace AoC2022.Day23 

open AoC2022.Utilities

type Direction =
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest

type Elf =
    {
        X: int
        Y: int
        ProposedDest: (int * int) option
    }

type Grove =
    {        
        Grid:  char [,]
        Elves: Elf list
        Round: int
    }

/// Grovde module for AoC Day 23.
module Grove =

    /// Helper function to redraw grid to cater for expanding elf positions.
    let private adjustGrid (grove: Grove) =
        let minx = grove.Elves |> List.map (fun e -> e.X) |> List.min
        let miny = grove.Elves |> List.map (fun e -> e.Y) |> List.min
        let maxx = grove.Elves |> List.map (fun e -> e.X) |> List.max
        let maxy = grove.Elves |> List.map (fun e -> e.Y) |> List.max
        let dx = if minx < 0 then -minx else 0
        let dy = if miny < 0 then -miny else 0
        let elves' =
            grove.Elves
            |> List.map (fun e ->
                let proposedDest' =
                    match e.ProposedDest with
                    | None -> None
                    | Some (pdx, pdy) -> (pdx+dx,pdy+dy) |> Some
                {e with X = e.X+dx; Y = e.Y+dy; ProposedDest=proposedDest'})
        let grid' =
            let contents = Array2D.init (maxx+dx+1) (maxy+dy+1) (fun _ _ -> '.')
            elves' |> List.iter (fun e -> contents.[e.X,e.Y] <- '#')
            contents
        {grove with Grid = grid'; Elves = elves'} 

    /// Helper function to find out if a coordinate is unoccupied.
    let private unoccupiedCoord (x,y) grove =
        let xs, ys = grove.Grid |> Array2D.length1, grove.Grid |> Array2D.length2
        // Entering new space.
        (x < 0 || x >= xs || y < 0 || y >= ys) ||
        // Or unoccupied.
        grove.Grid.[x,y] <> '#'

    /// Create a new grove for elves to move around.
    let create (field: string array) =
        let grid =
            let contents = Array2D.init field.[0].Length field.Length (fun _ _ -> '.')
            for y in [0..field.Length-1] do
                field.[y] |> Seq.iteri (fun i x -> contents.[i,y] <- x)
            contents
        let elves =
            [for x in [0..(grid |> Array2D.length1)-1] do
                for y in [0..(grid |> Array2D.length2)-1] ->
                    if grid.[x,y] = '#' then Some {X=x; Y=y; ProposedDest=None} else None]
            |> List.choose (fun e -> e)
        {
            Grid  = grid
            Elves = elves
            Round = 0
        }

    /// Check less than 8 surrounding positions are unoccupied which means an elf will want to move.
    let wantsToMove (elf: Elf) (grove: Grove) =
        let x, y = elf.X, elf.Y                       
        //N   ; NE     ; E    ; SE     ; S    ; SW     ; W    ; NW
        [x,y-1; x+1,y-1; x+1,y; x+1,y+1; x,y+1; x-1,y+1; x-1,y; x-1,y-1]
        |> List.map (fun c -> grove |> unoccupiedCoord c)        
        |> List.filter (fun u -> u = true)
        |> List.length
        |> (<>) 8

    /// Determine an elf's preferred move.
    let preferedMove (elf: Elf) (grove: Grove) =
        let canMove (direction: Direction) =
            let x, y = elf.X, elf.Y
            match direction with
            | North -> [x-1,y-1; x,y-1; x+1,y-1]
            | South -> [x-1,y+1; x,y+1; x+1,y+1] 
            | East  -> [x+1,y-1; x+1,y; x+1,y+1] 
            | West  -> [x-1,y-1; x-1,y; x-1,y+1]
            | _ -> failwith "Unsupported direction."
            |> List.map (fun c -> grove |> unoccupiedCoord c)
            |> List.reduce (&&)
        let preferences =        
            let directions = [|North; South; West; East|]
            [0..3] |> List.map (fun d -> directions.[(d+grove.Round) % 4])
        let x, y = elf.X, elf.Y
        let direction =
            preferences
            |> List.tryFind (fun p -> p |> canMove)
        match direction with
        | Some North -> {elf with ProposedDest = (x,y-1) |> Some}
        | Some South -> {elf with ProposedDest = (x,y+1) |> Some}
        | Some East  -> {elf with ProposedDest = (x+1,y) |> Some}
        | Some West  -> {elf with ProposedDest = (x-1,y) |> Some}
        | _ -> elf

    /// Move elves 1 round.
    let moveElves (grove: Grove) =
        let elvesToMove =
            grove.Elves
            |> List.filter (fun e -> grove |> wantsToMove e)
            |> List.map (fun e -> grove |> preferedMove e)
            |> List.groupBy (fun e -> e.ProposedDest)
            |> List.filter (fun (_,v) -> v |> List.length = 1)
            |> List.map (snd)
            |> List.collect (fun e -> e)
        let elves' =
            grove.Elves
            |> List.map (fun e ->
                elvesToMove
                |> List.tryFind (fun etm -> (etm.X, etm.Y) = (e.X, e.Y))
                |> function
                   | Some elf ->
                        match elf.ProposedDest with
                        | None -> e
                        | Some pd -> {e with X = pd |> fst; Y = pd |> snd; ProposedDest = Some pd}
                   | None -> e)
        {grove with Elves = elves'; Round = grove.Round+1} |> adjustGrid

    /// Move elves given number of rounds.
    let moveElvesNRounds n (grove: Grove) =
        [1..n]
        |> List.fold (fun g _ -> g |> moveElves) grove

    /// Returns true if elves have finished moving.
    let finishedMoving (grove: Grove) =
        grove.Elves
        |> List.map (fun e -> grove |> wantsToMove e)
        |> List.filter (fun e -> e = false)
        |> List.length
        |> (=) grove.Elves.Length

    /// Get empty ground in enclosing rectangle of elf positions.
    let getEmptyGroundCount (grove: Grove) =
        let minx, maxx, miny, maxy =
            let xs = grove.Elves |> List.map (fun e -> e.X)
            let ys = grove.Elves |> List.map (fun e -> e.Y)
            xs |> List.min, xs |> List.max, ys |> List.min, ys |> List.max            
        [for y in [miny..maxy] do
            for x in [minx..maxx] -> if grove.Grid.[x,y] = '.' then 1 else 0]
        |> List.sum

    /// Move elves as far as they want to.
    let rec moveElvesAllRounds (grove: Grove) =
        let grove' = grove |> moveElves
        if not (grove' |> finishedMoving) then            
            grove' |> moveElvesAllRounds            
        else
            grove'

    /// Display current grove configuration.
    let display (grove: Grove) =
        let minx, maxx, miny, maxy =
            let xs = grove.Elves |> List.map (fun e -> e.X)
            let ys = grove.Elves |> List.map (fun e -> e.Y)
            xs |> List.min, xs |> List.max, ys |> List.min, ys |> List.max            
        for y in [miny..maxy] do
            for x in [minx..maxx] do
                printf "%c" grove.Grid.[x,y]
            printfn ""