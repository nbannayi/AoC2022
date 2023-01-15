namespace AoC2022.Day12

open AoC2022.Utilities

type HillClimber =   
   {
       HillMap:          Grid
       StartPosition:    Coordinate
       EndPosition:      Coordinate
       VisitedPositions: bool array array
       PathParents     : Coordinate array array       
   }
   static member create hillMap =           
       let getPosition hillMap symbol =
           let pos =
               hillMap.Items
               |> Array.mapi (fun i r -> i, r |> Seq.tryFindIndex (fun c -> c = symbol))
               |> Array.filter (fun c -> snd c <> None)
               |> Array.head
           in {X = pos |> snd |> Option.get; Y = pos |> fst}       
       let startPosition = 'S' |> getPosition hillMap
       {
           HillMap          = hillMap
           StartPosition    = startPosition
           EndPosition      = 'E' |> getPosition hillMap
           VisitedPositions = Array.init hillMap.Height (fun _ -> Array.init hillMap.Width (fun _ -> false))
           PathParents = Array.init hillMap.Height (fun _ -> Array.init hillMap.Width (fun _ -> {X=0; Y=0}))
       }

/// Hill climber module for AoC day 12.
module HillClimber =
      
   /// Reset the paths for a new trek.
   let reset climber =
       {climber with
           VisitedPositions = Array.init climber.HillMap.Height (fun _ -> Array.init climber.HillMap.Width (fun _ -> false))
           PathParents = Array.init climber.HillMap.Height (fun _ -> Array.init climber.HillMap.Width (fun _ -> {X=0; Y=0}))}

   /// Get height at a landscape position.
   let getHeight (position: Coordinate) climber =
       climber.HillMap.Items.[position.Y].[position.X]

   /// Return true if too steep to traverse.
   let isTooSteep (current: Coordinate) next climber =
       let updateSymbol symbol =
           match symbol with
           | 'S' -> 'a'
           | 'E' -> 'z'
           | _ -> symbol
       let currentSymbol = climber |> getHeight current |> updateSymbol
       let nextSymbol    = climber |> getHeight next    |> updateSymbol
       (nextSymbol |> int) - (currentSymbol |> int) > 1
      
   /// Return a coordinate and whether we can move in that direction.
   let canMove direction (pos: Coordinate) climber =
       let (newPosition: Coordinate) =        
           match direction with
           | Up    -> {X = pos.X;   Y = pos.Y-1}
           | Down  -> {X = pos.X;   Y = pos.Y+1}
           | Left  -> {X = pos.X-1; Y = pos.Y}
           | Right -> {X = pos.X+1; Y = pos.Y}
           | _     -> failwith "Unsupported direction."
       match newPosition with
       | np   when np.Y < 0 || np.Y > climber.HillMap.Height-1 || np.X < 0 || np.X > climber.HillMap.Width-1 -> np, false       
       | _ -> newPosition, climber |> isTooSteep pos newPosition |> not
      
   /// Get all neighbours from given position.
   let getPossibleMoves position climber =       
       [Up; Right; Down; Left]
       |> List.map (fun dir ->
           let newPos, canMove = climber |> canMove dir position
           if canMove then Some newPos else None)
       |> List.choose (fun d -> d)       

   /// Set a position as visited.
   let setVisited (position: Coordinate) (climber: HillClimber) =
       climber.VisitedPositions.[position.Y].[position.X] <- true       

   /// Return true if a position has been visited.
   let hasVisited (position: Coordinate) (climber: HillClimber) =
       climber.VisitedPositions.[position.Y].[position.X]

   /// Change start position.
   let setStartPosition position (climber: HillClimber) =
       {climber with StartPosition = position}

   /// Return true if we have reached the end.
   let isEndPosition position (climber: HillClimber) =
       (climber |> getHeight position) = 'E'

   /// Breadth first search implementation to climb the mountains.
   let climb (climber: HillClimber) =       
       let queue = [|[climber.StartPosition]|]
       while queue |> Array.head <> List.empty do
           match (queue |> Array.head) with
           | head::tail ->
               queue.[0] <- tail
               if not (climber |> hasVisited head) then                                   
                   climber |> setVisited head
                   if not (climber |> isEndPosition head) then                                               
                       let neighbours = climber |> getPossibleMoves head
                       neighbours
                       |> List.filter (fun n -> not (climber |> hasVisited n))
                       |> List.iter (fun n -> climber.PathParents.[n.Y].[n.X] <- head)                                        
                       queue.[0] <- queue.[0] @ neighbours                   
           | [] -> ()
       climber

   /// Get shortest path after climbing the mountain.   
   let getShortestPath (climber: HillClimber) =            
       let rec getParent (position: Coordinate) climber =
           let path =
               seq {
                   let c = climber.PathParents.[position.Y].[position.X]                                   
                   if c <> climber.StartPosition then
                       yield! climber |> getParent c                                       
               }
           Seq.append path (seq {climber.EndPosition}) |> Array.ofSeq
       let hasSolution =
           [for x in [0..climber.HillMap.Height-1] do
               for y in [0..climber.HillMap.Width-1] ->
                   climber |> getHeight climber.PathParents.[x].[y] = 'z']
           |> List.contains true                   
       match hasSolution with
       | true  -> climber |> getParent climber.EndPosition
       | false -> Array.empty

   /// Get all lowest points on the landscape.
   let getAllLowestPoints (climber: HillClimber) =
       [for x in [0..climber.HillMap.Width-1] do
           for y in [0..climber.HillMap.Height-1] ->
               let symbol = climber.HillMap.Items.[y].[x]
               if symbol = 'S' || symbol = 'a' then Some {X=x;Y=y} else None]
       |> List.choose (fun p -> p)