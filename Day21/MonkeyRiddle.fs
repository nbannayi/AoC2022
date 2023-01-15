namespace AoC2022.Day21

open AoC2022.Utilities

type Yell =
   {
       Operand1: Operand
       Operand2: Operand option
       Operator: Operator option
   }
  
type Monkey =
   {
       Name: string
       Yell: Yell   
   }

module Monkey =

   let isComplete (monkey: Monkey) =
       if monkey.Name = "nick" then true else monkey.Yell.Operand2 = None
      
   let evaluate (monkey: Monkey) =
       let operand1', operand2', operator' =
           match monkey.Yell.Operand1, monkey.Yell.Operand2 with
           | Value(o1), Some(Value(o2)) ->
               let operator = monkey.Yell.Operator |> Option.get
               match operator with
               | Add      -> Value(o1+o2), None, None
               | Multiply -> Value(o1*o2), None, None
               | Subtract -> Value(o1-o2), None, None
               | Divide   -> Value(o1/o2), None, None
           | _ -> monkey.Yell.Operand1, monkey.Yell.Operand2, monkey.Yell.Operator
       {monkey with Yell={Operand1 = operand1'; Operand2 = operand2'; Operator = operator'}}

   let display (monkey: Monkey) =
       let operand1 = match monkey.Yell.Operand1 with Value(n) -> string n | Name(n) -> string n | _ -> " "
       let operand2 = match monkey.Yell.Operand2 with Some(Value(n)) -> string n | Some(Name(n)) -> string n | _ -> " "
       let operator = match monkey.Yell.Operator with Some(operator) -> (operator |> Operator.toString) | _ -> " "
       printfn "%s: %s %s %s" monkey.Name operand1 operator operand2

type MonkeyRiddle =
   {
       Monkeys: Monkey array
   }

/// More monkeys for day 21!
module MonkeyRiddle =

   /// Create a new monkey riddle.
   let create (riddleInput: string seq) =
       let monkeys =
           riddleInput
           |> Seq.map (fun r ->
               let tokens = r.Split ' '
               let name = tokens.[0].Replace(":","")
               let operand1 = tokens.[1] |> Operand.parse
               let operand2, operator =
                   match tokens.Length > 2 with
                   | true  -> Some (tokens.[3] |> Operand.parse), Some (tokens.[2] |> Operator.parse)
                   | false -> None, None
               {Name = name; Yell = {Operand1 = operand1; Operand2 = operand2; Operator = operator}})
           |> Array.ofSeq
       {Monkeys = monkeys}               
       
   /// Returns true if the riddle is completely solved.
   let isSolved (riddle: MonkeyRiddle) =
       riddle.Monkeys
       |> Array.forall (fun m -> m |> Monkey.isComplete)

   /// Update a single monkeys.
   let updateMonkey (monkey: Monkey) (riddle: MonkeyRiddle) =
       // Calculate passed monkey based on current MonkeyRiddle contents.
       let monkey' =
           if not (monkey |> Monkey.isComplete) then
               let operand1, operand2 = monkey.Yell.Operand1, monkey.Yell.Operand2
               let operand1' =
                   match operand1 with
                   | Name(name) ->
                       riddle.Monkeys
                       |> Array.tryFind (fun m -> m.Name = name)
                       |> function
                          | Some m' -> if m' |> Monkey.isComplete then m'.Yell.Operand1 else operand1
                          | None    -> operand1
                   | _ -> operand1
               let operand2' =
                   match operand2 with
                   | Some(Name(name)) ->
                       riddle.Monkeys
                       |> Array.tryFind (fun m -> m.Name = name)
                       |> function
                          | Some m' -> if m' |> Monkey.isComplete then Some m'.Yell.Operand1 else operand2
                          | None    -> operand2
                   | _ -> operand2
               {monkey with Yell={monkey.Yell with Operand1 = operand1'; Operand2 = operand2'}}
               |> Monkey.evaluate
           else
               monkey
       // Now update in monkeys collection.
       let monkeyIndex = riddle.Monkeys |> Array.findIndex (fun m -> m.Name = monkey.Name)
       riddle.Monkeys.[monkeyIndex] <- monkey'
       riddle
     
   /// Update all monkeys in one sweep.
   let updateMonkeys (riddle: MonkeyRiddle) =
       riddle.Monkeys |> Array.fold (fun r m -> r |> updateMonkey m) riddle

   /// Display all monkeys, for debugging.
   let display (riddle: MonkeyRiddle) =
       riddle.Monkeys |> Array.iter (fun m -> m |> Monkey.display)

   /// Solve by recursively processing monkeys.
   let rec solve (riddle: MonkeyRiddle) =
       if riddle |> isSolved then
           riddle
       else
           let initialMonkeys = Array.init riddle.Monkeys.Length (fun i -> riddle.Monkeys.[i])
           let riddle' = riddle |> updateMonkeys
           if riddle'.Monkeys <> initialMonkeys then riddle' |> solve else riddle'           
           
   /// Get the value at root for part 1.
   let getRootValue (riddle: MonkeyRiddle) =
       let root = riddle.Monkeys |> Array.find (fun m -> m.Name = "root")
       match root.Yell.Operand1 with
       | Value(n) -> n
       | _ -> -1L

   /// Remove the human for part 2 (well, rename it to my name.)
   let removeHuman (riddle: MonkeyRiddle) =
       let humnIndex = riddle.Monkeys |> Array.findIndex (fun m -> m.Name = "humn")
       riddle.Monkeys.[humnIndex] <- {riddle.Monkeys.[humnIndex] with Name = "nick"}
       riddle

   /// Obtain a monkey from the riddle by name.
   let getMonkeyByName name (riddle: MonkeyRiddle) =
       riddle.Monkeys |> Array.tryFind (fun m -> m.Name = name)

   /// Given a monkey and a target solve the equation of one unknown.
   let solveEquation (monkey: Monkey) target (riddle: MonkeyRiddle) =
       let operand1    = monkey.Yell.Operand1
       let operand2    = monkey.Yell.Operand2
       let operator    = monkey.Yell.Operator
       let currentName = monkey.Name
       match currentName, operand1, operand2, operator with
       | "root", Value(v), Some(Name(name)), _         -> riddle |> getMonkeyByName name, v
       | "root", Name(name), Some(Value(v)), _         -> riddle |> getMonkeyByName name, v
       | _, Name(name), Some(Value(v)), Some(Add)      -> riddle |> getMonkeyByName name, target-v
       | _, Name(name), Some(Value(v)), Some(Subtract) -> riddle |> getMonkeyByName name, target+v
       | _, Name(name), Some(Value(v)), Some(Multiply) -> riddle |> getMonkeyByName name, target/v
       | _, Name(name), Some(Value(v)), Some(Divide)   -> riddle |> getMonkeyByName name, target*v
       | _, Value(v), Some(Name(name)), Some(Add)      -> riddle |> getMonkeyByName name, target-v
       | _, Value(v), Some(Name(name)), Some(Subtract) -> riddle |> getMonkeyByName name, v-target
       | _, Value(v), Some(Name(name)), Some(Multiply) -> riddle |> getMonkeyByName name, target/v
       | _, Value(v), Some(Name(name)), Some(Divide)   -> riddle |> getMonkeyByName name, v/target
       | _ -> failwith "UNable to solve equation."

   /// Get human yell for part 2.  The strategy here is to find the root and work backwards
   /// successively solving linear equations till we get to human.
   let getHumanYell (riddle: MonkeyRiddle) =
       let rec getHumanYell monkey target riddle =       
           let (nextMonkey, target) = riddle |> solveEquation monkey target
           match nextMonkey with
           | Some m -> riddle |> getHumanYell m target
           | None -> target      
       let root = riddle |> getMonkeyByName "root" |> Option.get
       riddle |> getHumanYell root 0L
