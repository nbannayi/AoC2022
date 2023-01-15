open AoC2022.Utilities
 
open AoC2022.Day11
open System
 
// Advent of Code 2022 day 11.
[<EntryPoint>]
let main argv =
   
    let getGame () =
        {Monkeys =
            "InputFiles/Day11Input.txt"
            |> Seq.ofFileChunks "\n\n"
            |> Seq.map (fun m -> m.Split([|"\n"|], StringSplitOptions.None))
            |> Seq.map (fun m ->
                {
                    Id            = let tokens = m.[0].Split ' ' in tokens.[1].Replace(":","") |> int
                    StartingItems = m.[1].Trim().Replace("Starting items: ","").Split([|", "|], StringSplitOptions.None)
                                    |> List.ofArray
                                    |> List.map (int64)
                    Operation     =
                        let tokens   = m.[2].Trim().Split ' '
                        let operands = (tokens.[3] |> Operand.parse, tokens.[5] |> Operand.parse)
                        let operator = tokens.[4]  |> Operator.parse
                        Operation.create operands operator
                    Test          =                   
                        let divisibility   = let tokens = m.[3].Trim().Split ' ' in tokens.[3] |> int64
                        let throwToIfTrue  = let tokens = m.[4].Trim().Split ' ' in tokens.[5] |> int
                        let throwToIfFalse = let tokens = m.[5].Trim().Split ' ' in tokens.[5] |> int
                        Test.create divisibility throwToIfTrue throwToIfFalse                   
                    NoInspections = 0
                })  
            |> Array.ofSeq
        }
 
    let monkeyBusiness =
        getGame ()
        |> MonkeyGame.playRounds true 20
        |> MonkeyGame.getMonkeyBusiness
 
    printfn "Part 1 answer is: %d" monkeyBusiness
   
    let monkeyBusiness' =
        getGame ()
        |> MonkeyGame.playRounds false 10000
        |> MonkeyGame.getMonkeyBusiness
 
    printfn "Part 2 answer is: %d" monkeyBusiness'
    0