open AoC2022.Utilities

open AoC2022.Day22
open System

// Advent of Code 2022 day 22.
[<EntryPoint>]
let main argv =

   // Actual inputs.
   let cubeDetails =
       {           
           Boundaries = [|(50,0,99,49); (100,0,149,49); (50,50,99,99); (0,100,49,149); (50,100,99,149); (0,150,49,199)|]
           Wrappings  = [|(1,Up,6,Right); (2,Up,6,Up); (2,Right,5,Left); (2,Down,3,Left)
                          (3,Right,2,Up); (5,Right,2,Left); (5,Down,6,Left); (6,Right,5,Up)
                          (6,Down,2,Down); (6,Left,1,Down); (4,Left,1,Right); (4,Up,3,Right)
                          (3,Left,4,Down); (1,Left,4,Right)|]
       }
   (*
   // Example inputs.
   let cubeDetails =
       {           
           Boundaries = [|(8,0,11,3); (0,4,3,7); (4,4,7,7); (8,4,11,7); (8,8,11,11); (12,8,15,11)|]
           Wrappings  = [|(1,Up,2,Down);    (1,Right,6,Left); (4,Right,6,Down); (6,Up,4,Left)
                          (6,Right,1,Left); (6,Down,2,Right); (5,Down,2,Up);    (5,Left,3,Up)
                          (3,Down,5,Right); (2,Down,5,Up);    (2,Left,6,Up);    (2,Up,1,Down)
                          (3,Up,1,Right);   (1,Left,3,Down)|]
       }
   *)

   let map1, map2, moves =
       let inputs =
           "InputFiles/Day22Input.txt"
           |> Seq.ofFileChunks "\n\n"
           |> Array.ofSeq
       // Map 1 - net.  vv
       (inputs.[0] |> function m -> m.Split([|"\n"|], StringSplitOptions.None), None)
       ||> MonkeyMap.create,
       // Map 2 - cube. vv
       (inputs.[0] |> function m -> m.Split([|"\n"|], StringSplitOptions.None), Some cubeDetails)
       ||> MonkeyMap.create,
       // Monkey moves. vv
       inputs.[1].Replace("R",",R,").Replace("L",",L,").Split(',') |> Array.filter (fun m -> m <> "")
       |> Array.map (MonkeyMove.parse)

   let password =
       moves
       |> Array.fold (fun map move -> map |> MonkeyMap.monkeyMove false move) map1
       |> MonkeyMap.getPassword
  
   printfn "Part 1 answer is: %d" password   
       
   let password' =
       moves
       |> Array.fold (fun map move -> map |> MonkeyMap.monkeyMove true move) map2
       |> MonkeyMap.getPassword
      
   printfn "Part 2 answer is: %d" password'     
   0
