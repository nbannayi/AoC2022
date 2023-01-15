namespace AoC2022.Utilities

open System

/// General purpose cube.
type Cube =
   {
       X:      int
       Y:      int
       Z:      int
   }
   static member create (x, y, z) =
       {X = x; Y = y; Z = z}
      
module Cube =
      
   /// Return true if both cubes are touching (i.e. share a face), otherwise false.
   let isTouching (cube1: Cube) (cube2: Cube) =
       match cube1, cube2 with
       | c1, c2 when Math.Abs(c2.X - c1.X) = 1 && c2.Y = c1.Y && c2.Z = c1.Z -> true
       | c1, c2 when Math.Abs(c2.Y - c1.Y) = 1 && c2.X = c1.X && c2.Z = c1.Z -> true
       | c1, c2 when Math.Abs(c2.Z - c1.Z) = 1 && c2.X = c1.X && c2.Y = c1.Y -> true
       | _ -> false
  
   /// Get all positions adjacent to given cube position.
   let getAdjacentPositions (x, y, z) =
       [(x+1, y, z); (x-1,y,z); (x,y+1,z); (x,y-1,z); (x,y,z-1); (x,y,z+1)]
