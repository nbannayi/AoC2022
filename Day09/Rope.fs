namespace AoC2022.Day09 

open System 

/// Direction of motion when rope is pulled. 
type Direction =  
   | Up  
   | Right  
   | Down  
   | Left 
   /// Convert a char to a direction. 
   static member parse d = 
       match d with 
       | "U" -> Up 
       | "D" -> Down 
       | "L" -> Left 
       | "R" -> Right 
       | _   -> failwith "Unknown direction." 
    
/// 2D Coordinate in the plane. 
type Coordinate = {X: int; Y: int} 

/// Motion in which to move the rope. 
type Motion = Direction * int 

/// A tail segment that can be moved around the rope. 
type RopeSegment = 
   { 
       Id:  char 
       Pos: Coordinate 
   } 
   static member create id = 
       { 
           Id  = id 
           Pos = {X = 0; Y = 0} 
       } 

/// Represents a rope segment at a point in time. 
module RopeSegment = 
        
   /// Update tail position. 
   let updateTail (ropeSegments: RopeSegment array) = 
       let updateTailSegment n (ropeSegments: RopeSegment array) =  
           let areSpacedApart coord1 coord2 = Math.Abs(coord2.Pos.Y - coord1.Pos.Y) > 1 || Math.Abs(coord2.Pos.X - coord1.Pos.X) > 1 
           let leader, follower = ropeSegments.[n], ropeSegments.[n+1] 
           let followerNewPos = 
               match follower, leader with 
               // These cases where head and tail are in same row or same column. 
               | f, l when f.Pos.Y = l.Pos.Y && l.Pos.X > f.Pos.X && Math.Abs(l.Pos.X - f.Pos.X) > 1 -> {X = l.Pos.X-1; Y = l.Pos.Y} 
               | f, l when f.Pos.Y = l.Pos.Y && l.Pos.X < f.Pos.X && Math.Abs(l.Pos.X - f.Pos.X) > 1 -> {X = l.Pos.X+1; Y = l.Pos.Y} 
               | f, l when f.Pos.X = l.Pos.X && l.Pos.Y > f.Pos.Y && Math.Abs(l.Pos.Y - f.Pos.Y) > 1 -> {X = l.Pos.X;   Y = l.Pos.Y-1} 
               | f, l when f.Pos.X = l.Pos.X && l.Pos.Y < f.Pos.Y && Math.Abs(l.Pos.Y - f.Pos.Y) > 1 -> {X = l.Pos.X;   Y = l.Pos.Y+1} 
               // These cases where head and tail are not in same row or same column. 
               | f, l when l.Pos.Y > f.Pos.Y && l.Pos.X > f.Pos.X && areSpacedApart f l -> {X = f.Pos.X+1; Y = f.Pos.Y+1} 
               | f, l when l.Pos.Y > f.Pos.Y && l.Pos.X < f.Pos.X && areSpacedApart f l -> {X = f.Pos.X-1; Y = f.Pos.Y+1} 
               | f, l when l.Pos.Y < f.Pos.Y && l.Pos.X > f.Pos.X && areSpacedApart f l -> {X = f.Pos.X+1; Y = f.Pos.Y-1}                 
               | f, l when l.Pos.Y < f.Pos.Y && l.Pos.X < f.Pos.X && areSpacedApart f l -> {X = f.Pos.X-1; Y = f.Pos.Y-1}                 
               // In all other cases remain in the same position. 
               | _ -> follower.Pos           
           ropeSegments.[n]   <- leader 
           ropeSegments.[n+1] <- {follower with Pos = followerNewPos}   
           ropeSegments             
       (ropeSegments, [|0..ropeSegments.Length-2|]) ||> Array.fold (fun rs s -> rs |> updateTailSegment s) 

   /// Update head position bsed on direction given. 
   let updateHead direction (ropeSegment: RopeSegment) =         
       let newPos =  
           match direction with 
           | Right -> {X = ropeSegment.Pos.X+1; Y = ropeSegment.Pos.Y} 
           | Left  -> {X = ropeSegment.Pos.X-1; Y = ropeSegment.Pos.Y} 
           | Up    -> {X = ropeSegment.Pos.X;   Y = ropeSegment.Pos.Y+1} 
           | Down  -> {X = ropeSegment.Pos.X;   Y = ropeSegment.Pos.Y-1}                             
       {ropeSegment with Pos = newPos} 

/// Represents a rope at a point in time. 
type Rope = 
   {         
       Head:      RopeSegment      
       Tail:      RopeSegment array 
       TailId:    char 
       HeadTrail: Coordinate list     
       TailTrail: RopeSegment list     
   } 
   /// Create a new rope. 
   static member create noTailSegments = 
       let origin = {X = 0; Y = 0} 
       { 
           Head      = RopeSegment.create 'H' 
           Tail      = [|1..noTailSegments|] |> Array.map (fun n -> n |> (string >> char) |> RopeSegment.create) 
           TailId    = noTailSegments |> (string >> char) 
           HeadTrail = [origin] 
           TailTrail = List.empty 
       } 

/// Advent of Code 2022 day 9 Rope module. 
module Rope = 

   /// Returns true if tail has visited the given position. 
   let hasTailVisited x y rope = 
       rope.TailTrail |> List.exists (fun rs -> rs.Pos.X = x && rs.Pos.Y = y && rs.Id = rope.TailId) 

   /// Get number of positions tail has visited at least once. 
   let noTailVisited rope = 
       rope.TailTrail |> List.distinct |> List.length 
    
   /// Display the rope at a point in time. 
   /// S = start position, H = head position, T = tail position, # = tail trail, . = not visited. 
   let display (rope: Rope) =         
       let minX, maxX = (rope.HeadTrail |> List.minBy (fun c -> c.X)).X, (rope.HeadTrail |> List.maxBy (fun c -> c.X)).X 
       let minY, maxY = (rope.HeadTrail |> List.minBy (fun c -> c.Y)).Y, (rope.HeadTrail |> List.maxBy (fun c -> c.Y)).Y 
       [for y in [maxY..(-1).. minY] do for x in [minX..maxX] -> (x, y)] 
       |> List.iter (fun (x,y) -> 
           match x, y with 
           | x, y when x = rope.Head.Pos.X && y = rope.Head.Pos.Y                      -> printf "H"   
           | x, y when rope.Tail |> Array.exists (fun t -> t.Pos.X = x && t.Pos.Y = y) -> printf "T" 
           | x, y when x = 0 && y = 0                                                  -> printf "s"        
           | x, y when rope |> hasTailVisited x y                                      -> printf "#" 
           | _                                                                         -> printf "." 
           if x = maxX then printfn "") 
       printfn "\n" 

   /// Move rope by 1 unit in given direction. 
   let moveUnit direction (rope: Rope) =               
       let head      = rope.Head |> RopeSegment.updateHead direction 
       let tail      = (Array.append [|head|] rope.Tail) |> RopeSegment.updateTail |> Array.skip 1 
       let headTrail = List.append rope.HeadTrail [head.Pos]                 
       let tailTrail =  
           let tail' = tail |> Array.filter (fun t -> t.Id = rope.TailId) |> List.ofArray 
           (List.append rope.TailTrail tail') |> List.distinct 
       {rope with Head = head; Tail = tail; HeadTrail = headTrail; TailTrail = tailTrail}         
    
   /// Move rope by n units in given direction. 
   let move (motion: Motion) (rope: Rope) = 
       let (direction, units) = motion                 
       (rope, [1..units]) ||> List.fold (fun r _ -> r |> moveUnit direction) 
