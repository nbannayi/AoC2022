namespace AoC2022.Day22
 
open AoC2022.Utilities
open System
 
type Monkey =
    {
        Position: int * int
        Facing:   Direction       
    }
 
// Specific rather than general solution, my monkey brain is too dumb to figure out a general one :/
type CubeDetails =
    {
        // Specifies boundary of each cube from top left to bottom right.
        Boundaries: (int * int * int * int) array
        // Specifies how to wrap from each face to the next.
        Wrappings: (int * Direction * int * Direction) array
    }
 
type MonkeyMap =
    {
        // X=right, Y=down.
        Grid:        char [,]
        Width:       int
        Height:      int
        Monkey:      Monkey
        CubeDetails: CubeDetails option
    }
 
type MonkeyMove =
    | Direction of Direction   
    | Forward   of int
    static member parse (move: string) =
        match move |> Int32.TryParse with
        | true, result -> Forward(result)
        | _ -> match move with
               | "L" -> Direction(Left)
               | "R" -> Direction(Right)
               | "D" -> Direction(Down)
               | "U" -> Direction(Up)
               | _   -> failwith "Invalid move."
   
module MonkeyMap =
 
    /// Updates the position of the monkey on the map.
    let private updateMonkeyPosition (map: MonkeyMap) =
        let monkeyChar =
            match map.Monkey.Facing with
            | Up    -> '^'
            | Right -> '>'
            | Down  -> 'v'
            | Left  -> '<'
            | _ -> failwith "Invalid monkey facing."
        let x, y = map.Monkey.Position
        map.Grid.[x,y] <- monkeyChar
        map
            
    /// Create a new monkey map.
    let create (mapInput: string array) (cubeDetails: CubeDetails option) =
        let width  = mapInput |> Array.map (fun l -> l.Length) |> Array.max
        let height = mapInput.Length
        let grid   = Array2D.init width height (fun _ _ -> ' ')
        for y in [0..height-1] do
            for x in [0..width-1] do
                if x < mapInput.[y].Length then grid.[x, y] <- mapInput.[y].[x]
        let monkey =
            {
                Position =
                    [0..width-1]
                    |> List.find (fun x -> mapInput.[0].[x] = '.')
                    |> function x -> (x,0)
                Facing = Right       
            }       
        {Grid = grid; Width = width; Height = height; Monkey = monkey; CubeDetails = cubeDetails}
        |> updateMonkeyPosition
 
    /// Arbitrarily set start pos, useful for debugging.
    let setStartPosition (x,y) (map: MonkeyMap) =
        {map with Monkey = {map.Monkey with Position = (x,y)}}
        |> updateMonkeyPosition
 
    /// Get allocated face for a specific position.
    let getCubeFace (x,y) (map: MonkeyMap) =
        match map.CubeDetails with
        | None -> failwith "No cube details provided for this input."
        | Some details ->
           details.Boundaries |> Array.tryFindIndex (fun b ->
                let bx1,by1,bx2,by2 = b
                x >= bx1 && x <= bx2 && y >= by1 && y <= by2)
            |> function
               | Some i -> i+1
               | None   -> 0
   
    /// Get mind bending wrapping rules for a cube.
    // Note this function is specific to the input, since my brain hurts too much even after making a cube from paper!   
    let findCubeWrap cubeFace direction (x,y) (map: MonkeyMap) =
        let cubeFace', direction', boundaries =
            match map.CubeDetails with
            | None -> failwith "No cube details provided for this input."
            | Some details ->
                let f, d =
                    details.Wrappings
                    |> Array.find (fun w -> let f1, d1, _, _ = w in cubeFace = f1 && direction = d1)
                    |> function _, _, f2, d2 -> f2, d2
                let b = details.Boundaries.[f-1]
                f,d,b       
        let bx1,by1,bx2,by2 = boundaries
        let interval =
            // Debuging this was very, very tough, day 22 was the "hell puzzle" vvv
            match cubeFace', direction' with
            | 6, Right -> [bx1..bx2]       |> List.map (fun x' -> x', 100+x)
            | 6, Up    -> [by2..(-1)..by1] |> List.map (fun y' -> x-100, y')
            | 5, Left  -> [bx2..(-1)..bx1] |> List.map (fun x' -> x', 149-y)
            | 3, Left  -> [bx2..(-1)..bx1] |> List.map (fun x' -> x', x-50)
            | 2, Up    -> [by2..(-1)..by1] |> List.map (fun y' -> 50+y, y')
            | 2, Left  -> [bx2..(-1)..bx1] |> List.map (fun x' -> x', 149-y)
            | 6, Left  -> [bx2..(-1)..bx1] |> List.map (fun x' -> x', 100+x)
            | 5, Up    -> [by2..(-1)..by1] |> List.map (fun y' -> y-100, y')
            | 2, Down  -> [by1..by2]       |> List.map (fun y' -> 100+x, y')
            | 1, Down  -> [by1..by2]       |> List.map (fun y' -> y-100, y')
            | 1, Right -> [bx1..bx2]       |> List.map (fun x' -> x', 149-y) 
            | 3, Right -> [bx1..bx2]       |> List.map (fun x' -> x', 50+x)
            | 4, Down  -> [by1..by2]       |> List.map (fun y' -> y-50, y')
            | 4, Right -> [bx1..bx2]       |> List.map (fun x' -> x', 149-y)           
            | _ -> failwith "Unsupported transition."           
            (*
            // Example code - not fully tested.
            match cubeFace', direction' with
            | 1, Down  -> [by1..by2]       |> List.map (fun y' -> bx2-(3-x), y')
            | 1, Left  -> [bx2..(-1)..bx1] |> List.map (fun x' -> x', x-3)
            | 4, Left  -> [bx2..(-1)..bx1] |> List.map (fun x' -> x', 15-x)
            | 6, Down  -> [by1..by2]       |> List.map (fun y' -> 19-y, y') // tested
            | 6, Left  -> [bx2..(-1)..bx1] |> List.map (fun x' -> x', 6+y)
            | 6, Up    -> [by2..(-1)..by1] |> List.map (fun y' -> 6+y, y')
            | 5, Up    -> [by2..(-1)..by1] |> List.map (fun y' -> 9-x, y')
            | 5, Right -> [bx1..bx2]       |> List.map (fun x' -> x', 3+x)
            | 3, Up    -> [by2..(-1)..by1] |> List.map (fun y' -> 15-y, y')
            | 2, Up    -> [by2..(-1)..by1] |> List.map (fun y' -> 11-x, y') // tested
            | 2, Right -> [bx1..bx2]       |> List.map (fun x' -> x', x-3) 
            | 2, Down  -> [by1..by2]       |> List.map (fun y' -> 9-x, y')
            | 3, Down  -> [by1..by2]       |> List.map (fun y' -> 3+y, y')
            | 1, Right -> [bx1..bx2]       |> List.map (fun x' -> x', x-4)  // tested
            | _ -> failwith "Unsupported transition."
            *)
        let (x',y') = interval |> List.skipWhile (fun (x,y) -> map.Grid.[x,y] = ' ') |> List.head
        if map.Grid.[x',y'] <> '#' then Some (direction', (x',y')) else None
 
    /// Part 1 moving around the net of the cube.
    let private processMoveNet direction (x, y) map =
        let wrap = x < 0 || x > map.Width-1 || y < 0 || y > map.Height-1 || map.Grid.[x,y] = ' '
        let wall = if wrap then false else map.Grid.[x,y] = '#'
        let findWrap list =
            let (x,y) = list |> List.skipWhile (fun (x,y) -> map.Grid.[x,y] = ' ') |> List.head
            if map.Grid.[x,y] <> '#' then Some (x,y) else None
        match direction, (x, y), wrap, wall with
        | Up,    _, _, true -> Up,    (x, y+1) // Blocked at wall move back
        | Down,  _, _, true -> Down,  (x, y-1) // ""
        | Right, _, _, true -> Right, (x-1, y) // ""
        | Left,  _, _, true -> Left,  (x+1, y) // ""
                                               // Wrap-around but move back if wrap-around block is a wall vvv
        | Up,   (x, y), true, _ -> Up,   [(map.Height-1)..(-1)..0] |> List.map (fun y' -> x, y') |> findWrap |> Option.defaultValue (x, y+1)
        | Down, (x, y), true, _ -> Down, [0..(map.Height-1)]       |> List.map (fun y' -> x, y') |> findWrap |> Option.defaultValue (x, y-1)
        | Right,(x, y), true, _ -> Right,[0..(map.Width-1)]        |> List.map (fun x' -> x', y) |> findWrap |> Option.defaultValue (x-1, y)
        | Left, (x, y), true, _ -> Left, [(map.Width-1)..(-1)..0]  |> List.map (fun x' -> x', y) |> findWrap |> Option.defaultValue (x+1, y)
        | _ -> direction, (x,y)
 
    /// Part 2 moving around the cube (folded net.)
    let private processMoveCube direction (x, y) map =       
        let wrap = x < 0 || x > map.Width-1 || y < 0 || y > map.Height-1 || map.Grid.[x,y] = ' '
        let wall = if wrap then false else map.Grid.[x,y] = '#'           
        match direction, (x, y), wrap, wall with
        | Up,    _, _, true -> Up,    (x, y+1) // Blocked at wall move back
        | Down,  _, _, true -> Down,  (x, y-1) // ""
        | Right, _, _, true -> Right, (x-1, y) // ""
        | Left,  _, _, true -> Left,  (x+1, y) // ""
                                                  // Wrap-around but move back if wrap-around block is a wall vvv
        | Up,   (x, y), true, _ -> map |> findCubeWrap (map |> getCubeFace (x, y+1)) Up (x, y) |> Option.defaultValue (Up, (x, y+1))
        | Down, (x, y), true, _ -> map |> findCubeWrap (map |> getCubeFace (x, y-1)) Down  (x, y) |> Option.defaultValue (Down, (x, y-1))
        | Right,(x, y), true, _ -> map |> findCubeWrap (map |> getCubeFace (x-1, y)) Right (x, y) |> Option.defaultValue (Right, (x-1, y))
        | Left, (x, y), true, _ -> map |> findCubeWrap (map |> getCubeFace (x+1, y)) Left  (x, y) |> Option.defaultValue (Left, (x+1, y))
        | _ -> direction, (x,y)
 
    /// Move the monkey in the map.
    let monkeyMove isCube (move: MonkeyMove) (map: MonkeyMap) =   
        let monkeyMove' processMove move map =   
            let facing, (x,y) = map.Monkey.Facing, map.Monkey.Position
            let facing', position' =
                match facing, move with
                | Up,    Direction(Right) | Down,  Direction(Left) -> Right, (x,y)
                | Right, Direction(Right) | Left,  Direction(Left) -> Down,  (x,y)
                | Down,  Direction(Right) | Up,    Direction(Left) -> Left,  (x,y)
                | Left,  Direction(Right) | Right, Direction(Left) -> Up,    (x,y)
                | Up,    Forward(_) -> map |> processMove Up    (x, y-1)
                | Down,  Forward(_) -> map |> processMove Down  (x, y+1)
                | Left,  Forward(_) -> map |> processMove Left  (x-1, y)
                | Right, Forward(_) -> map |> processMove Right (x+1, y)
                | _ -> failwith "Invalid monkey move."
            {map with Monkey = {map.Monkey with Facing = facing'; Position = position'}}
            |> updateMonkeyPosition
        let processMove = if isCube then processMoveCube else processMoveNet
        match move with
        | Forward(n) -> [1..n] |> List.fold (fun mm _ -> mm  |> monkeyMove' processMove move) map
        | Direction(Left) | Direction(Right)          -> map |> monkeyMove' processMove move 
        | _ -> failwith "Unrecognised move."
 
    /// Get final password based on position and facing.
    let getPassword (map: MonkeyMap) =
        let positionValue = let x,y = map.Monkey.Position in 1000*(y+1) + 4*(x+1)
        let facingValue =
            match map.Monkey.Facing with
            | Right -> 0
            | Down  -> 1
            | Left  -> 2
            | Up    -> 3
            | _     -> failwith "Unknown facing."
        positionValue+facingValue
        
    /// Display the monkey map/path in all it/s glory.
    let display (map: MonkeyMap) =       
        for y in [0..map.Height-1] do
            for x in [0..map.Width-1] do
                // Debug - uncomment to check correct face allocation.
                //printf "%d" (map |> getCubeFace (x,y))
                printf "%c" map.Grid.[x, y]
            printfn ""       
 