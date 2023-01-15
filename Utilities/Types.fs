namespace AoC2022.Utilities

open System

/// General purpose grid.
type Grid =
   {
       Items:  string array
       Height: int
       Width:  int   
   }
   static member create grid =
       {
           Items  = grid
           Width  = grid.[0].Length
           Height = grid.Length       
       }

/// General-purpose direction.
type Direction =
   | Up
   | Right
   | Down
   | Left
   | Forward of int
   | Backward of int
   /// Convert a char to a direction.
   static member parse d =
       match d with
       | "U" -> Up
       | "D" -> Down
       | "L" -> Left
       | "R" -> Right
       | "F" -> Forward(1)
       | "B" -> Backward(1)
       | _   -> failwith "Unknown direction."

/// Represents an Operator in Monkey operation definition.
type Operator =
   | Add
   | Multiply
   | Divide
   | Subtract
   static member parse operator =
       match operator with
       | "+" -> Add
       | "*" -> Multiply
       | "/" -> Divide
       | "-" -> Subtract
       | _   -> failwith "Unsupported operator."
   static member toString operator =
       match operator with
       | Add      -> "+"
       | Multiply -> "*"
       | Divide   -> "/"
       | Subtract -> "-"

/// Represents an operand in Monkey operation definition.
type Operand  =
   | New
   | Old
   | Value of int64
   | Name of string
   static member parse operand =
       match operand with
       | "new"  -> New
       | "old"  -> Old
       | operand when operand |> Int32.TryParse |> fst -> Value(operand |> int64)
       | _ as operand -> Name(operand)      

/// Represents an operation in Monkey definition.
type Operation =
   {
       Operands: Operand * Operand
       Operator: Operator       
   }
   static member create operands operator =
       {
           Operands = operands
           Operator = operator
       }

/// Represents an operation in monkey definition.
module Operation =

   /// Applies an operation.
   let apply n operation =
       let operand2 = match operation.Operands |> snd with | Value(v) -> v | _ -> n           
       match operation.Operator with
       | Add      -> n + operand2
       | Multiply -> n * operand2
       | Divide   -> n / operand2
       | Subtract -> n - operand2

/// 2D Coordinate in the plane.
type Coordinate = {X: int; Y: int}

/// 3D Coordinate in space.
type Coordinate3D = {X: int; Y: int; Z: int}

/// Motion in which to move.
type Motion = Direction * int
