open AoC2022.Day10 

// Advent of Code 2022 day 10. 
[<EntryPoint>] 
let main argv = 
    
   let cathodeRayTube = CathodeRayTube.create "InputFiles/Day10Input.txt" 

   let signalStrengthSum = 
       [20; 60; 100; 140; 180; 220] 
       |> List.map (fun c ->  
           cathodeRayTube  
           |> CathodeRayTube.runToCycle c 
           |> CathodeRayTube.getX 
           |> (*) c) 
       |> List.sum 
    
   printfn "Part 1 answer is: %d" signalStrengthSum 
        
   cathodeRayTube  
   |> CathodeRayTube.reset 
   |> CathodeRayTube.run  
   |> CathodeRayTube.displayScreen 
    
   printfn "Part 2 answer is: %s" "FCJAPJRE" 
   0