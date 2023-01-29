namespace AoC2022.Day16

open System.Collections.Generic

type Valve =
    {
        Label:    string
        Id:       int
        FlowRate: int
        Tunnels:  string list
    }
    static member create id (valveLine: string) =
        let tokens   = valveLine.Split ' '
        let label    = tokens.[1]
        let flowRate = (tokens.[4].Split '=').[1].Replace(";","") |> int
        let tunnels  = tokens.[9..] |> Array.toList |> List.map (fun t -> t.Replace(",",""))
        {Label = label; Id = id; FlowRate = flowRate; Tunnels = tunnels}        

type ValveSystem =
    {
        Valves           : Map<string,Valve>
        ValvesWithFlow   : Set<string>
        ShortestDistances: int [,]
        Paths            : Dictionary<int,int>
    }

module ValveSystem =

    /// Create a valve system. 
    let create (valveLines: string seq) =        
        let valves =
            valveLines
            |> Seq.sortBy (fun vl -> vl.[6..7])
            |> Seq.mapi (fun i vl -> Valve.create i vl)
            |> Seq.map (fun v -> v.Label, v)
            |> Map.ofSeq
        /// Get shortest distances between all valves in the valve system using Floyd-Walshall algorithm.        
        let distances =            
            let n = valves.Count
            let distances' = Array2D.create n n 9999 // Initialise with a large sentinel value.            
            [0..n-1] |> List.iter (fun i -> distances'.[i,i] <- 0)            
            valves
            |> Map.toList
            |> List.iter (fun (_,valve) ->
                let id1, tunnels = valve.Id, valve.Tunnels 
                tunnels |> List.iter (fun t ->
                    let id2 = valves.[t].Id in distances'.[id1,id2] <- 1))                                                
            for k in [0..n-1] do
                for i in [0..n-1] do
                    for j in [0..n-1] do                
                        if distances'.[i,j] > distances'.[i,k] + distances'.[k,j] then
                            distances'.[i,j] <- distances'.[i,k] + distances'.[k,j]
            distances'
        {
            Valves            = valves
            ValvesWithFlow    = valves |> Map.toList |> List.filter (fun (_,v) -> v.FlowRate > 0) |> List.map (fst) |> Set
            ShortestDistances = distances
            Paths             = new Dictionary<int,int>()
        }

    /// Encode a unique id of path for cacheing.
    let encodePath (path: (string*int) list) valveSystem =
        path |> List.skip 1
             |> List.fold (fun a (label,_) ->
                let id = valveSystem.Valves.[label].Id in a ||| (1 <<< id)) 0

    /// Get and cache cost of passed path.
    let getPressure (path: (string*int) list) timeOut valveSystem =
        let score =
            path
            |> List.map (fun (v,t) -> let flow = valveSystem.Valves.[v].FlowRate in (timeOut - t) * flow)
            |> List.sum
        let ep = valveSystem |> encodePath path
        if not (valveSystem.Paths.ContainsKey(ep)) then
            valveSystem.Paths.Add(ep, score)
        elif score > valveSystem.Paths.[ep] then
            valveSystem.Paths.[ep] <- score

    /// Get shortest path length from distances calculated.
    let getPathLength (distances: int [,]) a b valveSystem =
        let id1 = valveSystem.Valves.[a].Id
        let id2 = valveSystem.Valves.[b].Id
        distances.[id1,id2]

    /// Traverse pipes to get largest pressure.
    let rec traverse (path: (string*int) list) turn timeOut valveSystem =            
        let current,_ = path |> List.last   
        let visited = path |> List.map (fun (v,_) -> v) |> Set.ofList            
        let available = Set.difference valveSystem.ValvesWithFlow visited        
        getPressure path timeOut valveSystem            
        if available.Count > 0 then
            for next in available do                        
                let cost = (valveSystem |> getPathLength valveSystem.ShortestDistances current next) + 1
                if turn + cost < timeOut then
                    traverse (path @ [next, turn+cost]) (turn + cost) timeOut valveSystem