open AoC2022.Utilities
open System

// Advent of Code 2022 day 3.
[<EntryPoint>]
let main argv =
    
    let rucksacks =
        "InputFiles/Day03Input.txt" 
        |> Seq.ofFileLines
        |> Seq.map (fun c -> 
            let compartmentsSize = (c.Length/2) - 1 
            c.[0..compartmentsSize], c.[compartmentsSize+1..])        
            
    let getItemScore item =
        match Char.IsLower(item) with
        | true  -> (item |> int) - 96
        | false -> (item |> int) - 38

    let commonItemsScore1 =
        let getCommonItem compartment1 compartment2 =
            (compartment1 |> Set.ofSeq) 
            |> Set.intersect (compartment2 |> Set.ofSeq)
            |> Set.toSeq |> Seq.head
        rucksacks
        |> Seq.map (fun (compartment1, compartment2) -> getCommonItem compartment1 compartment2)
        |> Seq.sumBy (getItemScore)

    printfn "Part 1 answer is: %d" commonItemsScore1

    let commonItemsScore2 =
        let getCommonItem (rucksacks: (string * string) array) =        
            let combineItems rucksack = 
                (rucksack |> fst) + (rucksack |> snd)         
            seq [rucksacks.[0] |> combineItems |> Set.ofSeq;
                 rucksacks.[1] |> combineItems |> Set.ofSeq;
                 rucksacks.[2] |> combineItems |> Set.ofSeq]
            |> Set.intersectMany
            |> Set.toSeq |> Seq.head
        rucksacks
        |> Seq.chunkBySize 3
        |> Seq.map (fun rucksacks -> rucksacks |> getCommonItem)
        |> Seq.sumBy (getItemScore)

    printfn "Part 2 answer is: %d" commonItemsScore2
    0