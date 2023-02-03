namespace AoC2022.Day19

open System.Collections.Generic
open System

type RobotState =
    {
        Blueprint:          Blueprint
        Minute:             int
        OreCount:           int
        ClayCount:          int
        ObsidianCount:      int
        GeodeCount:         int
        OreRobotCount:      int
        ClayRobotCount:     int
        ObsidianRobotCount: int
        GeodeRobotCount:    int
    }

type BuildOption =
    | Ore
    | Clay
    | Obsidian
    | Geode
    | Wait

module RobotState =

    let create blueprint =
        {
            Blueprint          = blueprint
            Minute             = 0
            OreCount           = 0          
            ClayCount          = 0         
            ObsidianCount      = 0     
            GeodeCount         = 0        
            OreRobotCount      = 1     
            ClayRobotCount     = 0    
            ObsidianRobotCount = 0
            GeodeRobotCount    = 0            
        }

    // The functions below are quite repetitive and could use a refactor. v

    /// Build ore robot.
    let private buildOreRobot maxMinutes robotState =
        let blueprint = robotState.Blueprint
        let remainingMinutes = maxMinutes-robotState.Minute
        if robotState.OreCount + (robotState.OreRobotCount * remainingMinutes) >= blueprint.OreRobotCost then 
            let minutesRequired =
                Math.Ceiling(double (blueprint.OreRobotCost-robotState.OreCount)/(double robotState.OreRobotCount)) |> int
            let oreCount =
                robotState.OreCount + (minutesRequired * robotState.OreRobotCount) - blueprint.OreRobotCost
                + robotState.OreRobotCount
            let clayCount =
                robotState.ClayCount + (minutesRequired * robotState.ClayRobotCount)
                + robotState.ClayRobotCount
            let obsidianCount =
                robotState.ObsidianCount + (minutesRequired * robotState.ObsidianRobotCount)
                + robotState.ObsidianRobotCount
            let geodeCount =
                robotState.GeodeCount + (minutesRequired * robotState.GeodeRobotCount)
                + robotState.GeodeRobotCount
            let oreRobotCount = robotState.OreRobotCount+1            
            let minute = robotState.Minute + minutesRequired + 1
            {robotState with
                OreCount = oreCount; ClayCount = clayCount; ObsidianCount = obsidianCount; GeodeCount = geodeCount;
                OreRobotCount = oreRobotCount; Minute = minute}
        else
            robotState

    /// Build clay robot.
    let private buildClayRobot maxMinutes robotState =
        let blueprint = robotState.Blueprint
        let remainingMinutes = maxMinutes-robotState.Minute
        if robotState.OreCount + (robotState.OreRobotCount * remainingMinutes) >= blueprint.ClayRobotCost then
            let minutesRequired =
                Math.Ceiling(double (blueprint.ClayRobotCost-robotState.OreCount)/(double robotState.OreRobotCount)) |> int                
            let oreCount =
                robotState.OreCount + (minutesRequired * robotState.OreRobotCount) - blueprint.ClayRobotCost
                + robotState.OreRobotCount
            let clayCount =
                robotState.ClayCount + (minutesRequired * robotState.ClayRobotCount)
                + robotState.ClayRobotCount
            let obsidianCount =
                robotState.ObsidianCount + (minutesRequired * robotState.ObsidianRobotCount)
                + robotState.ObsidianRobotCount
            let geodeCount =
                robotState.GeodeCount + (minutesRequired * robotState.GeodeRobotCount)
                + robotState.GeodeRobotCount
            let clayRobotCount = robotState.ClayRobotCount+1            
            let minute = robotState.Minute + minutesRequired + 1
            {robotState with
                OreCount = oreCount; ClayCount = clayCount; ObsidianCount = obsidianCount; GeodeCount = geodeCount;
                ClayRobotCount = clayRobotCount; Minute = minute}
        else
            robotState

    /// Build obsidian robot.
    let private buildObsidianRobot maxMinutes robotState =
        let blueprint = robotState.Blueprint
        let remainingMinutes = maxMinutes-robotState.Minute
        if robotState.OreCount + (robotState.OreRobotCount * remainingMinutes) >= blueprint.ObsidianRobotCost.OreCost &&
           robotState.ClayCount + (robotState.ClayRobotCount * remainingMinutes) >= blueprint.ObsidianRobotCost.ClayCost then
            let minutesRequired =
                let minsOre  = Math.Ceiling(double (blueprint.ObsidianRobotCost.OreCost-robotState.OreCount)/(double robotState.OreRobotCount)) |> int
                let minsClay = Math.Ceiling(double (blueprint.ObsidianRobotCost.ClayCost-robotState.ClayCount)/(double robotState.ClayRobotCount)) |> int
                [minsOre; minsClay] |> List.max
            let oreCount =
                robotState.OreCount + (minutesRequired * robotState.OreRobotCount) - blueprint.ObsidianRobotCost.OreCost
                + robotState.OreRobotCount
            let clayCount =
                robotState.ClayCount + (minutesRequired * robotState.ClayRobotCount) - blueprint.ObsidianRobotCost.ClayCost
                + robotState.ClayRobotCount
            let obsidianCount =
                robotState.ObsidianCount + (minutesRequired * robotState.ObsidianRobotCount)
                + robotState.ObsidianRobotCount
            let geodeCount =
                robotState.GeodeCount + (minutesRequired * robotState.GeodeRobotCount)
                + robotState.GeodeRobotCount
            let obsidianRobotCount = robotState.ObsidianRobotCount + 1
            let minute = robotState.Minute + minutesRequired + 1
            {robotState with
                OreCount = oreCount; ClayCount = clayCount; ObsidianCount = obsidianCount; GeodeCount = geodeCount;
                ObsidianRobotCount = obsidianRobotCount; Minute = minute}            
        else
            robotState

    /// Build geode robot.
    let private buildGeodeRobot maxMinutes robotState =
        let blueprint = robotState.Blueprint
        let remainingMinutes = maxMinutes-robotState.Minute
        if robotState.OreCount + (robotState.OreRobotCount * remainingMinutes) >= blueprint.GeodeRobotCost.OreCost &&
           robotState.ObsidianCount + (robotState.ObsidianRobotCount * remainingMinutes) >= blueprint.GeodeRobotCost.ObsidianCost then
            let minutesRequired =
                let minsOre = Math.Ceiling(double (blueprint.GeodeRobotCost.OreCost-robotState.OreCount)/(double robotState.OreRobotCount)) |> int
                let minsObsidian = Math.Ceiling(double (blueprint.GeodeRobotCost.ObsidianCost-robotState.ObsidianCount)/(double robotState.ObsidianRobotCount)) |> int
                [minsOre; minsObsidian] |> List.max
            let oreCount =
                robotState.OreCount + (minutesRequired * robotState.OreRobotCount) - blueprint.GeodeRobotCost.OreCost
                + robotState.OreRobotCount
            let clayCount =
                robotState.ClayCount + (minutesRequired * robotState.ClayRobotCount)
                + robotState.ClayRobotCount
            let obsidianCount =
                robotState.ObsidianCount + (minutesRequired * robotState.ObsidianRobotCount) - blueprint.GeodeRobotCost.ObsidianCost
                + robotState.ObsidianRobotCount
            let geodeCount =
                robotState.GeodeCount + (minutesRequired * robotState.GeodeRobotCount)
                + robotState.GeodeRobotCount
            let geodeRobotCount = robotState.GeodeRobotCount + 1
            let minute = robotState.Minute + minutesRequired + 1
            {robotState with
                OreCount = oreCount; ClayCount = clayCount; ObsidianCount = obsidianCount; GeodeCount = geodeCount;
                GeodeRobotCount = geodeRobotCount; Minute = minute}
        else
            robotState

    /// Just collect ores if a robot can't be built.
    let private collect robotState =        
        {
            robotState with
                OreCount      = robotState.OreCount      + robotState.OreRobotCount
                ClayCount     = robotState.ClayCount     + robotState.ClayRobotCount
                ObsidianCount = robotState.ObsidianCount + robotState.ObsidianRobotCount
                GeodeCount    = robotState.GeodeCount    + robotState.GeodeRobotCount
                Minute        = robotState.Minute        + 1
        }

    /// Build robot selected.
    let buildRobot buildOption maxMinutes robotState =
        match buildOption with
        | Ore      -> buildOreRobot      maxMinutes robotState
        | Clay     -> buildClayRobot     maxMinutes robotState
        | Obsidian -> buildObsidianRobot maxMinutes robotState
        | Geode    -> buildGeodeRobot    maxMinutes robotState
        | Wait     -> collect                       robotState

    /// Choose build options at each step.
    let getBuildOptions robotState =
        let blueprint = robotState.Blueprint
        let maxSpendRate =
            [blueprint.OreRobotCost; blueprint.ClayRobotCost; blueprint.ObsidianRobotCost.OreCost; blueprint.GeodeRobotCost.OreCost]
            |> List.max
        // Only build an Ore robot if we can afford it and the number of Ore robots is below the max spend rate.
        let buildOreRobot      = robotState.OreCount >= blueprint.OreRobotCost && robotState.OreRobotCount < maxSpendRate
        // Only build a clay robot if we can afford it and the number of clay robots is less than the obsidian robot cost.
        let buildClayRobot     = robotState.OreCount >= blueprint.ClayRobotCost && robotState.ClayRobotCount < blueprint.ObsidianRobotCost.ClayCost
        // Only build an obsidian robot is we can afford it and the number of obsidian robots is less than the geode obsidian cost.
        let buildObsidianRobot =
            robotState.OreCount >= blueprint.ObsidianRobotCost.OreCost && robotState.ClayCount >= blueprint.ObsidianRobotCost.ClayCost &&
            robotState.ObsidianRobotCount < blueprint.GeodeRobotCost.ObsidianCost
        // Akways build a geode robot if we can afford it.
        let buildGeodeRobot =
            robotState.OreCount >= blueprint.GeodeRobotCost.OreCost && robotState.ObsidianCount >= blueprint.GeodeRobotCost.ObsidianCost
        
        [buildOreRobot; buildClayRobot; buildObsidianRobot; buildGeodeRobot; true]
        |> List.zip [Ore; Clay; Obsidian; Geode; Wait]
        |> List.filter (fun (_,b) -> b = true)
        |> List.map (fst)

    /// Diusplay current state, useful for debugging.
    let display robotState =
        printfn "Minute: %d - Ore Rocks/Robots: %d/%d, Clay Rocks/Robots: %d/%d, Obsidian Rocks/Robots: %d/%d, Geode Rocks/Robots: %d/%d"
            robotState.Minute
            robotState.OreCount      robotState.OreRobotCount      robotState.ClayCount  robotState.ClayRobotCount
            robotState.ObsidianCount robotState.ObsidianRobotCount robotState.GeodeCount robotState.GeodeRobotCount

    /// Get max geodes using BFS with memoisation.
    let getMaxGeodes maxMinutes robotState =        
        let queue = new Queue<RobotState*int>([robotState,0])
        let cache = new Dictionary<RobotState*BuildOption,int>()
        seq {
            while queue.Count > 0 do
                let robotState', minutes = queue.Dequeue()
                match minutes = maxMinutes with
                | true ->
                    queue.Clear()                    
                | false ->                                                         
                    for bo in robotState' |> getBuildOptions do                                            
                        if cache.ContainsKey(robotState',bo) then
                            yield cache.[robotState',bo]
                        else
                            let rs = robotState' |> buildRobot bo maxMinutes
                            yield rs.GeodeCount                            
                            cache.Add((robotState',bo),rs.GeodeCount)
                            queue.Enqueue(rs,minutes+1)
         } |> Seq.max