namespace AoC2022.Day17

open AoC2022.Utilities
open System

type TetrisType =
   | HorizLine
   | Cross
   | LShape
   | VertLine
   | Square

type TetrisBrick =
   {
       Type:     TetrisType
       Position: int * int
       Height:   int
       Landed:   bool   
   }
   static member create brickType =
       let height =
           match brickType with
           | HorizLine      -> 1
           | Square         -> 2
           | Cross | LShape -> 3
           | VertLine       -> 4
       {Type = brickType; Height = height; Position = 3,0; Landed = false}
      
type TetrisChamber =
   {
       Grid:          char [,]
       Height:        int
       CurrentTetris: TetrisBrick
       MovementCycle: Direction array
       CycleIndex:    int
       NoBricks:      int64
   }
          
// Tetris module for AoC day 17.
module TetrisChamber =

   /// Work out what brick to get next.
   let private getNextTetrisBrick currentTetris =
       let nextType =
           match currentTetris.Type with
           | HorizLine -> Cross
           | Cross     -> LShape
           | LShape    -> VertLine
           | VertLine  -> Square
           | Square    -> HorizLine
       TetrisBrick.create nextType

   /// Get next direction required.
   let private getNextCycleDirection chamber =
       let index = chamber.CycleIndex % chamber.MovementCycle.Length
       chamber.MovementCycle.[index]

   /// Get Teris coords based on shape to draw it.
   let private getTetrisCoords (x, y) chamber =
       match chamber.CurrentTetris.Type with
       | HorizLine -> [(x,y); (x+1,y); (x+2,y); (x+3,y)]
       | Cross     -> [(x+1,y); (x,y+1); (x+1,y+1); (x+2,y+1); (x+1,y+2)]
       | LShape    -> [(x+2,y); (x+2,y+1); (x,y+2); (x+1,y+2); (x+2,y+2)]
       | VertLine  -> [(x,y); (x,y+1); (x,y+2); (x,y+3)]
       | Square    -> [(x,y); (x+1,y); (x,y+1); (x+1,y+1)]

   /// Update Tetris chamber display.
   let private drawTetris chamber =
       // Clear previous Tetris position.
       for y in [0..chamber.Height-1] do           
           for x in [0..8] do
               if chamber.Grid.[x,y] = '@' then chamber.Grid.[x,y] <- '.'       
       // Determine if moving or landed.
       let tetrisChar = if chamber.CurrentTetris.Landed then '#' else '@'       
       // Draw Tetris based on type.
       (chamber |> getTetrisCoords chamber.CurrentTetris.Position)
       |> List.iter (fun (x,y) -> chamber.Grid.[x,y] <- tetrisChar)
       chamber
              
   /// Collision detection for both walls and bricks.
   let private detectCollision (x,y) chamber =
       // Check collision with walls.
       let canMove (x, y) chamber =
           let tetris = chamber.CurrentTetris
           let pastLeftBoundary = x < 1
           let pastRightBoundary =
               match tetris.Type with
               | HorizLine       -> x+3 > 7
               | LShape | Cross  -> x+2 > 7          
               | Square          -> x+1 > 7
               | VertLine        -> x   > 7
           let pastFloor =
               match tetris.Type with
               | HorizLine               -> y   > chamber.Height-2               
               | Cross | LShape | Square -> y+2 > chamber.Height-2               
               | VertLine                -> y+3 > chamber.Height-2
           [pastLeftBoundary; pastRightBoundary; pastFloor] |> List.reduce (||) |> not
       if chamber |> canMove (x, y) then        
           // Check collision with other bricks.
           chamber       
           |> getTetrisCoords (x, y)
           |> List.map (fun (x,y) -> chamber.Grid.[x,y] = '#')
           |> List.reduce (||)  
       else
           true
      
   /// Get level of bricks reached.
   let getLevel chamber =       
       let highestBrick =
           seq {for y in [0..chamber.Height-1] do           
                   for x in [0..8] -> (x,y)}
           |> Seq.tryFind (fun (x,y) -> chamber.Grid.[x,y] = '#')
       match highestBrick with
       | None       -> 0L
       | Some brick -> (chamber.Height - (snd brick) - 1) |> int64

   /// Move left, right or down as given.
   let moveDirection direction chamber =
       // Get new position and cycle index.
       let tetris', newCycleIndex =
           let x, y = chamber.CurrentTetris.Position
           let x', y', landed, cycleIndex' =                            
               match x, y, direction with
               | x, y, Left  -> if not (chamber |> detectCollision (x-1, y)) then x-1, y, false, chamber.CycleIndex+1 else x, y, false, chamber.CycleIndex+1
               | x, y, Right -> if not (chamber |> detectCollision (x+1, y)) then x+1, y, false, chamber.CycleIndex+1 else x, y, false, chamber.CycleIndex+1
               | x, y, Down  -> if not (chamber |> detectCollision (x, y+1)) then x, y+1, false, chamber.CycleIndex   else x, y, true,  chamber.CycleIndex
               | _ -> failwith "Direction not supported."           
           {chamber.CurrentTetris with Position = x', y'; Landed = landed}, cycleIndex'
       // Adjust chamber height if required.       
       let chamber' =
           if direction = Down then
               let xc, yc  = tetris'.Position
               let height' = Math.Max(1 + (chamber |> getLevel |> int), chamber.Height - yc)
               let newGrid, newPos =
                   if chamber.Height <> height' then 
                       let grid'   = Array2D.init 9 height' (fun _ _ -> '.')
                       for y in [0..height'-1] do           
                           for x in [0..8] do
                               grid'.[x,y] <- chamber.Grid.[x,y+yc]    
                       grid', 0
                   else
                       chamber.Grid, yc
               {chamber with Height = height'; Grid = newGrid; CurrentTetris = {tetris' with Position = xc,newPos}}               
           else
               {chamber with CurrentTetris = tetris'}           
           |> drawTetris
       // Draw new tetris if required.
       if chamber'.CurrentTetris.Landed then
           let currentTetris' = chamber'.CurrentTetris |> getNextTetrisBrick 
           let height' = chamber'.Height + currentTetris'.Height + 3
           let grid'   = Array2D.init 9 height' (fun _ _ -> '.')
           for y in [0..height'-1] do           
               for x in [0..8] do
                   if y > currentTetris'.Height + 2 then
                       grid'.[x,y] <- chamber'.Grid.[x,y-currentTetris'.Height-3]
           {chamber' with Height = height'; Grid = grid'; CycleIndex = newCycleIndex; CurrentTetris = currentTetris'; NoBricks = chamber.NoBricks+1L}           
       else
           {chamber' with CycleIndex = newCycleIndex}
       |> drawTetris

   /// Create a new Tetris chamber.
   let create brickType movementCycle =
       let currentTetris = TetrisBrick.create brickType
       {
           Grid          = Array2D.init 9 5 (fun _ _ -> '.')
           Height        = 5
           CurrentTetris = currentTetris
           MovementCycle = movementCycle
           CycleIndex    = 0
           NoBricks      = 0L
       }

   /// Move 1 cycle.
   let move chamber =
       let direction =
           chamber
           |> getNextCycleDirection       
       chamber
       |> moveDirection direction
       |> moveDirection Down

   /// Move N cycles in given direction.
   let moveNDirection n direction chamber =
       [1..n]
       |> List.fold (fun c _ -> c |> moveDirection direction) chamber       

   /// Display Tetris chamber.
   let display chamber =        
       printfn "No. bricks: %d, Level: %d, CycleIndex = %d" chamber.NoBricks (chamber |> getLevel) chamber.CycleIndex
       let floor = chamber.Height-1
       for y in [0..chamber.Height-1] do           
           for x in [0..8] do
               let c =
                   match x, y with
                   | 0, (_ as y') when y' = floor -> '+'
                   | 8, (_ as y') when y' = floor -> '+'
                   | 0, _ | 8, _                  -> '|'
                   | _, (_ as y') when y' = floor -> '-'                   
                   | _ -> chamber.Grid.[x,y]
               printf "%c" c
           printfn ""

   /// Fill chamber to given number of bricks.
   let fillToNoBricks n chamber =
       let rec fillToNoBricks' chamber =
           //printfn "Brick: %d" chamber.NoBricks
           let chamber' = chamber |> move
           if chamber'.NoBricks < n then chamber' |> fillToNoBricks' else chamber'
       chamber |> fillToNoBricks'

   /// Fill chamber to given level.
   let fillToLevel (n: int64) chamber =
       let rec fillToLevel' chamber =
           let chamber' = chamber |> move
           let level = chamber' |> getLevel
           //printfn "Level: %d" level
           if level < n then chamber' |> fillToLevel' else chamber'           
       chamber |> fillToLevel'

   /// Fill chamber to given number of cycles.
   let fillToNoCycles n chamber =
       [1..n]
       |> List.fold (fun c _ -> c |> move) chamber