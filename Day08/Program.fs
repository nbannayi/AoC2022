open AoC2022.Utilities 

// Advent of Code 2022 day 8. 
type Direction = Up | Right | Down | Left 
type TreeGrid = 
   { 
       Trees:  string array 
       Height: int 
       Width:  int     
   } 
   static member create grid =  
       { 
           Trees  = grid 
           Width  = grid.[0].Length 
           Height = grid.Length         
       } 

[<EntryPoint>] 
let main argv = 
    
   let treeGrid = 
       "InputFiles/Day08Input.txt"  
       |> Seq.ofFileLines 
       |> Array.ofSeq 
       |> TreeGrid.create 
           
   let getTreeVisibility x y (treeGrid: TreeGrid) = 
       let getTreeVisibilityByDirection x y (direction: Direction) (treeGrid: TreeGrid) =                 
           match x, y, direction with 
           | x, _, _ when x = 0 || x = treeGrid.Width-1  -> true // Left and right edge always visible. 
           | _, y, _ when y = 0 || y = treeGrid.Height-1 -> true // Top and bottom edge always visible. 
           | x, y, direction -> 
               let currentTree = treeGrid.Trees.[y].[x] |> Char.toInt 
               let otherTrees = 
                   match direction with 
                   | Up    -> [|0..y-1|]                   |> Array.map (fun yi -> treeGrid.Trees.[yi].[x] |> Char.toInt) 
                   | Right -> [|x+1..(treeGrid.Width-1)|]  |> Array.map (fun xi -> treeGrid.Trees.[y].[xi] |> Char.toInt) 
                   | Down  -> [|y+1..(treeGrid.Height-1)|] |> Array.map (fun yi -> treeGrid.Trees.[yi].[x] |> Char.toInt) 
                   | Left  -> [|0..x-1|]                   |> Array.map (fun xi -> treeGrid.Trees.[y].[xi] |> Char.toInt)                 
               (otherTrees |> Array.max) < currentTree  
           | _ -> false 
       [|Up;Right;Down;Left|] 
       |> Array.map (fun d -> treeGrid |> getTreeVisibilityByDirection x y d) 
       |> Array.reduce (||) 
                
   let getTreeScenicScore x y (treeGrid: TreeGrid) = 
       let currentTree = treeGrid.Trees.[y].[x] |> Char.toInt 
       let getTreeScenicScoreByDirection x y (direction: Direction) (treeGrid: TreeGrid) =                             
           let otherTrees = 
               match direction with 
               | Up    -> [|y-1..(-1)..0|]             |> Array.map (fun yi -> treeGrid.Trees.[yi].[x] |> Char.toInt) 
               | Right -> [|x+1..(treeGrid.Width-1)|]  |> Array.map (fun xi -> treeGrid.Trees.[y].[xi] |> Char.toInt) 
               | Down  -> [|y+1..(treeGrid.Height-1)|] |> Array.map (fun yi -> treeGrid.Trees.[yi].[x] |> Char.toInt) 
               | Left  -> [|x-1..(-1)..0|]             |> Array.map (fun xi -> treeGrid.Trees.[y].[xi] |> Char.toInt) 
           otherTrees  
           |> Array.tryFindIndex (fun t -> t >= currentTree)                
           |> function | None -> otherTrees.Length | Some i -> i+1         
       [|Up;Right;Down;Left|] 
       |> Array.map (fun d -> treeGrid |> getTreeScenicScoreByDirection x y d) 
       |> Array.reduce (*)         

   let noVisibleTrees =  
       [for x in [0..treeGrid.Width-1] do 
           for y in [0..treeGrid.Height-1] -> (x,y)] 
       |> List.map (fun (x,y) -> treeGrid |> getTreeVisibility x y) 
       |> List.filter (fun v -> v = true) 
       |> List.length 

   printfn "Part 1 answer is: %d" noVisibleTrees 

   let highestScenicScore = 
       [for x in [0..treeGrid.Width-1] do 
           for y in [0..treeGrid.Height-1] -> (x,y)] 
       |> List.map (fun (x,y) -> treeGrid |> getTreeScenicScore x y) 
       |> List.max 

   printfn "Part 2 answer is: %d" highestScenicScore     
   0 