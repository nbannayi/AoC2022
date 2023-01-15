namespace AoC2022.Day11

open AoC2022.Utilities

/// Represents a test in Monkey definition.
type Test =
   {
       Divisibility:   int64
       ThrowToIfTrue:  int
       ThrowToIfFalse: int
   }
   static member create divisibility throwToIfTrue throwToIfFalse =
       {
           Divisibility   = divisibility
           ThrowToIfTrue  = throwToIfTrue
           ThrowToIfFalse = throwToIfFalse
       }

/// Represents a test in Monkey definition.
module Test =

   /// Applies a divisibility test.
   let apply n test =
       if n % test.Divisibility = 0L then
           test.ThrowToIfTrue
       else
           test.ThrowToIfFalse

/// Represents a monkey in game.
type Monkey =
   {
       Id:            int
       StartingItems: int64 list
       Operation:     Operation
       Test:          Test
       NoInspections: int
   }

/// Monkey module for day 11.
module Monkey =
  
   /// Handle adjustment when monkey gets bored.
   let getBored limitWorry limiter item =
       match limitWorry with
       | true  -> item / 3L
       | false -> item % limiter
              
   /// Inspect (and throw) next item.
   let inspectNextItem limitWorry limiter monkey =
       match monkey.StartingItems with
       | [] -> monkey, None, None
       | item::rest ->                    
           let item' =
               monkey.Operation
               |> Operation.apply item
               |> getBored limitWorry limiter           
           let throwTo = monkey.Test |> Test.apply item'
           {monkey with StartingItems = rest; NoInspections = monkey.NoInspections+1}, Some item', Some throwTo

   /// Receives an item from another monkey.
   let receiveItem item (monkey: Monkey) =
       {monkey with StartingItems = monkey.StartingItems @ [item]}
