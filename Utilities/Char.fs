namespace AoC2022.Utilities

module Char =

   /// Convert a char to an int rather than the associated ASCII code.
   let toInt c =
       c |> (string >> int)