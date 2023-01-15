open AoC2022.Utilities
 
// Advent of Code 2022 day 18.
[<EntryPoint>]
let main argv =
   
    let cubes =
        "InputFiles/Day18Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (fun c ->
            let tokens = c.Split ','
            let x,y,z = tokens.[0] |> int, tokens.[1] |> int, tokens.[2] |> int
            Cube.create(x, y, z))       
 
    let getSurfaceArea cubes =
        cubes
        |> Seq.map (fun b -> b, cubes |> Seq.filter (fun b' -> b' |> Cube.isTouching b))
        |> Seq.map (fun (b,b') -> b, 6-(b' |> Seq.length))
        |> Seq.sumBy (snd)    
 
    printfn "Part 1 answer is: %d" (cubes |> getSurfaceArea)
 
    let getExteriorSurfaceArea (cubes: Cube seq) =
        let minx, maxx = (cubes |> Seq.minBy (fun c -> c.X)).X, (cubes |> Seq.maxBy (fun c -> c.X)).X
        let miny, maxy = (cubes |> Seq.minBy (fun c -> c.Y)).Y, (cubes |> Seq.maxBy (fun c -> c.Y)).Y
        let minz, maxz = (cubes |> Seq.minBy (fun c -> c.Z)).Z, (cubes |> Seq.maxBy (fun c -> c.Z)).Z
   
        let rec floodFill cubes cubeList =
            let outOfBounds (x, y, z) =
                (x < minx || x > maxx) || (y < miny || y > maxy) || (z < minz || z > maxz)           
            match cubeList with
            | [] -> cubes
            | h::_ when h |> outOfBounds -> Set.empty
            | h::t ->               
                h
                |> Cube.getAdjacentPositions
                |> List.filter (fun p -> cubes |> Set.contains p |> not)
                |> fun l -> floodFill (cubes |> Set.add h) (l @ t)
 
        let cubes' =
            cubes
            |> Seq.map (fun c -> (c.X, c.Y, c.Z))
            |> Set.ofSeq
       
        let airCubes =
            [for x in minx..maxx do
                for y in miny..maxy do
                    for z in minz..maxz do
                        if not (cubes' |> Set.contains (x, y, z)) then
                            (x, y, z)]               
        
        let allCubes =
            (cubes', airCubes)
            ||> Seq.fold (fun c ac ->
                match floodFill c [ac] with
                | s when Set.isEmpty s -> c
                | s -> s)                   
        
        allCubes
        |> Seq.map (fun (x,y,z) -> {Cube.X=x;Y=y;Z=z})
        |> getSurfaceArea
       
    printfn "Part 2 answer is: %d" (cubes |> getExteriorSurfaceArea)
    0