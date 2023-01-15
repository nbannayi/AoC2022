namespace AoC2022.Day14

open AoC2022.Utilities

type SandGridItem =
   | Rock
   | SandSource
   | Sand
   | Empty

type SandDirection =
   | Down
   | LeftDown
   | RightDown
   | Rest
   | Halt

type SandGrid =
   {
       Grid:     char [,]
       XOffset:  int
       MaxX    : int
       MaxY    : int
       IsHalted: bool
   }

/// Sany module for AoC day 14.
module SandGrid =

   /// Create the sand grid.
   let create paths =       
       let flattenedPaths = paths |> Seq.collect (fun p -> p)
       let minx, _ = flattenedPaths |> Seq.minBy (fun fp -> fst fp)
       let maxx, _ = flattenedPaths |> Seq.maxBy (fun fp -> fst fp)
       let _, maxy = flattenedPaths |> Seq.maxBy (fun fp -> snd fp)
       let width = maxx-minx
       {Grid = (Array2D.init (maxy+1) (width+1) (fun _ _ -> '.')); XOffset = minx; MaxX = maxx; MaxY = maxy; IsHalted = false}

   /// Plot a point on the sand grid.
   let plotPoint (x, y) (item: SandGridItem) (grid: SandGrid) =
       let itemChar =
           match item with
           | Rock       -> '#'
           | Sand       -> 'o'
           | SandSource -> '+'
           | _          -> '?'
       grid.Grid.[y, x-grid.XOffset] <- itemChar
       grid

   /// Plot the sand source.
   let plotSandSource (grid: SandGrid) =
       grid |> plotPoint (500,0) SandSource

   /// Plot a line in the sand grid.
   let plotLine (x1, y1) (x2, y2) (grid: SandGrid) =
       let getLinePoints (x1, y1) (x2, y2) =
           match x1, x2, y1, y2 with
           | x1, x2, _, _ when x1 = x2 ->
               let lineInterval = if y2 >= y1 then [y1..y2] else [y2..y1]
               lineInterval |> List.map (fun i -> (x1, i))
           | _, _, y1, y2 when y1 = y2 ->
               let lineInterval = if x2 >= x1 then [x1..x2] else [x2..x1]
               lineInterval |> List.map (fun i -> (i, y1))
           | _ -> failwith "Unexpected line input."
       let points = getLinePoints (x1, y1) (x2, y2)
       (grid, points) ||> List.fold (fun g p -> g |> plotPoint p Rock)
              
   /// Plot a path in the sand grid.
   let plotPath path (grid: SandGrid) =
       let lines = path |> Array.pairwise
       (grid, lines) ||> Array.fold (fun g (p1,p2) -> g |> plotLine p1 p2) 

   /// Plot a collection of paths.
   let plotPaths paths (grid: SandGrid) =
       (grid, paths) ||> Array.fold (fun g p -> g |> plotPath p) 

   /// Get item at a specific point in the grid.
   let getGridItem (x, y) (grid: SandGrid) =              
       match grid.Grid.[y, x-grid.XOffset] with
       | '.' -> Empty
       | '#' -> Rock
       | '+' -> SandSource
       | 'o' -> Sand
       | _ -> failwith "Unknown item."

   /// Get next possible move for the sand.
   let getNextSandMove (x, y) (grid: SandGrid) =
       if (x-1 < grid.XOffset || x+1 > grid.MaxX || (grid |> getGridItem (500,0)) = Sand) then
           Halt
       else
           let down      = grid |> getGridItem (x, y+1)
           let leftDown  = grid |> getGridItem (x-1, y+1)
           let rightDown = grid |> getGridItem (x+1, y+1)
           match down, leftDown, rightDown with
           | Empty, _, _ -> Down
           | _, Empty, _ -> LeftDown
           | _, _, Empty -> RightDown
           | _           -> Rest

   /// Move all the sand.
   let rec moveSand (grid: SandGrid) =       
       let move1Sand (grid: SandGrid) =                  
           let rec moveSand' (x, y) (grid: SandGrid) =       
               let nextMove = grid |> getNextSandMove (x,y)           
               match nextMove with
               | Down      -> grid |> moveSand' (x,y+1)
               | LeftDown  -> grid |> moveSand' (x-1,y+1)
               | RightDown -> grid |> moveSand' (x+1,y+1)
               | Rest      -> grid |> plotPoint (x, y) Sand           
               | Halt      -> {grid with IsHalted = true}
           grid |> moveSand' (500,0)
       if grid.IsHalted then
           grid
       else
           grid |> move1Sand |> moveSand
  
   /// Count number of grains of sand.
   let countSand (grid: SandGrid) =
       [for c in 0..(grid.Grid |> Array2D.length1)-1 do
           for r in 0..(grid.Grid |> Array2D.length2)-1 do
               if grid.Grid.[c,r] = 'o' then 1]
       |> List.sum
       
   /// Display sand grid.
   let display (grid: SandGrid) =
       for c in 0..(grid.Grid |> Array2D.length1)-1 do
           for r in 0..(grid.Grid |> Array2D.length2)-1 do
               printf "%c" grid.Grid.[c,r]
           printfn ""