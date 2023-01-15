namespace AoC2022.Day13

open System

type Ordering =
   | Equal
   | LeftRight
   | RightLeft
   | Undecided

type Signal =
   | Single of int
   | List of Signal list
   | EndMarker of int

/// Signal module for AoC day 13.
module Signal =
   
   /// Parse a string based signal into a nested Signal type.
   let parseSignal (signal: string) =
       let removeEndMarker signal = signal |> List.filter (fun e -> match e with EndMarker _ -> false | _ -> true)       
       let rec parseSignal' i (signal: string) =           
           match signal.[i] with      
           | ',' -> signal |> parseSignal' (i+1)
           | ']' -> [EndMarker i]
           | d when Char.IsDigit(d) ->
               if Char.IsDigit(signal.[i+1]) then           
                   [Single (signal.[i..i+1] |> int)] @ (signal |> parseSignal' (i+2))
               else
                   [Single (signal.[i] |> (string >> int))] @ (signal |> parseSignal' (i+1))
           | '[' ->
               let nestedSignal = signal |> parseSignal' (i+1)
               let endPos = match nestedSignal |> List.last with EndMarker pos -> pos | _ -> failwith "Unexpected nesting."
               [List (removeEndMarker nestedSignal)] @ (signal |> parseSignal' (endPos+1))             
           | _ -> failwith "Unknown character."
       List (signal |> parseSignal' 1 |> removeEndMarker)

   /// Compare two signals and return ordering from smallest to highest.
   let compare signal1 signal2 =
       let rec compare' signal1 signal2 i =
           let unpack signal = match signal with List [] -> [] | List l -> l | _ -> failwith "Unexpected nesting."       
           let l, r = signal1 |> unpack, signal2 |> unpack
           match l, r with
           | l, r when i >= l.Length && i <  r.Length -> LeftRight
           | l, r when i <  l.Length && i >= r.Length -> RightLeft       
           | l, r when i >= l.Length && i >= r.Length -> Equal
           | l, r ->
               match l.[i] with
               | Single l' ->
                   match r.[i] with
                   | Single r' -> if l' < r' then LeftRight elif l' > r' then RightLeft else compare' signal1 signal2 (i+1)
                   | List _    ->
                       let result = compare' (List [Single l']) r.[i] 0
                       match result with
                       | Equal -> compare' signal1 signal2 (i+1)
                       | _ -> result
                   | _ -> failwith "Unexpected element."
               | List _ ->
                   let result =
                       match r.[i] with
                       | Single r'   -> compare' l.[i] (List [Single r']) 0
                       | List _      -> compare' l.[i] r.[i] 0
                       | EndMarker _ -> failwith "Unexpected element."
                   match result with
                   | Equal -> compare' signal1 signal2 (i+1)
                   | _     -> result
               | _ -> failwith "Unexpectwed element."
       compare' signal1 signal2 0

   /// Sort a passed array of signals.
   // (Yes, it's a not very efficient bubble sort but should be ok here.)
   let rec sort (signals: Signal array) =
       let swaps =
           [0..signals.Length-2]
           |> List.map (fun n ->
               let ordering = compare signals.[n] signals.[n+1]
               match ordering with
               | RightLeft ->
                   let signalTemp = signals.[n]
                   signals.[n]   <- signals.[n+1]
                   signals.[n+1] <- signalTemp
                   true
               | _ -> false)
       if swaps |> List.contains (true) then
           signals |> sort
       else
           signals