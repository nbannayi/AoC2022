open AoC2022.Utilities 

// Advent of Code 2022 day 7. 
type File     = {Name: string; Size: int} 
type FilePath = {DirectoryPath: string; File: File option} 

[<EntryPoint>] 
let main argv = 
    
   let commands = 
        "InputFiles/Day07Input.txt"  
        |> Seq.ofFileLines 
        |> Array.ofSeq 
    
   let processCommand command filePath = 
       match command with 
       | "$ cd /" -> {DirectoryPath = "/"; File = None} 
       | "$ ls"   -> {DirectoryPath = filePath.DirectoryPath; File = None} 
       | command when command.[0..2] = "dir" ->             
           {DirectoryPath = filePath.DirectoryPath; File = None} 
       | "$ cd .." ->  
           let directoryPath' =                  
               let p = filePath.DirectoryPath |> Seq.findIndexBack (fun p -> p = '/') 
               let newDirectory = filePath.DirectoryPath.[0..p-1] 
               match newDirectory with | "" -> "/" | _ -> newDirectory 
           {DirectoryPath = directoryPath'; File = None} 
       | command when command.[0..3] = "$ cd" -> 
           let directoryPath' =  
               match filePath.DirectoryPath with 
               | "/" -> sprintf "%s%s"  filePath.DirectoryPath command.[5..] 
               | _   -> sprintf "%s/%s" filePath.DirectoryPath command.[5..] 
           {DirectoryPath = directoryPath'; File = None} 
       | _ ->  
           let tokens = command.Split ' ' 
           {DirectoryPath = filePath.DirectoryPath; File = {Name = tokens.[1]; Size = tokens.[0] |> int} |> Some} 

   let results =  
       ({DirectoryPath = "/"; File = None}, commands) ||> Array.scan (fun fp c -> fp |> processCommand c) 
       |> Array.filter (fun fp -> fp.File <> None)     
    
   let directoriesSummary =             
       let noLevels = 
           results 
           |> Array.map (fun r -> r.DirectoryPath.[1..].Split '/' |> Array.length) 
           |> Array.max         
       let directoriesByLevel n = 
           results  
           |> Array.filter (fun fp -> fp.DirectoryPath.[1..].Split '/' |> Array.length >= n)         
           |> Array.map (fun fp -> fp, fp.DirectoryPath.[1..].Split '/' |> Array.take n |> String.concat "/") 
           |> Array.groupBy (snd) 
           |> Array.map (fun (k,v) -> k, v |> Array.sumBy (fun (v',_) -> v'.File.Value.Size)) 
       [|1..noLevels|]  
       |> Array.map (directoriesByLevel) 
       |> Array.collect (fun d -> d) 

   let totalOver100k =  
       directoriesSummary 
       |> Array.filter (fun (_,s) -> s <= 100000) 
       |> Array.sumBy (snd) 

   printfn "Part 1 answer is: %d" totalOver100k 
    
   let smallestDirectoryNeeded = 
       let totalSpaceNeeded = 30000000 - (70000000 - (results |> Array.sumBy (fun fp -> fp.File.Value.Size))) 
       directoriesSummary 
       |> Array.filter (fun (_,s) -> s >= totalSpaceNeeded) 
       |> Array.sortBy (snd) 
       |> Array.head 
       |> snd 

   printfn "Part 2 answer is: %d" smallestDirectoryNeeded 
   0 