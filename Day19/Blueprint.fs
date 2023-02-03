namespace AoC2022.Day19

type ObsidianRobotCost =
    {
        OreCost : int
        ClayCost: int
    }

type GeodeRobotCost =
    {
        OreCost     : int
        ObsidianCost: int
    }

type Blueprint =
    {
        Id               : int
        OreRobotCost     : int
        ClayRobotCost    : int
        ObsidianRobotCost: ObsidianRobotCost
        GeodeRobotCost   : GeodeRobotCost
    }

module Blueprint =

    let create (input: string) =
        let tokens                 = input.Split ' '
        let id                     = tokens.[1].Replace(":","") |> int
        let oreRobotCost           = tokens.[6]                 |> int
        let clayRobotCost          = tokens.[12]                |> int
        let obsidianRobotOreCost   = tokens.[18]                |> int
        let obsidianRobotClayCost  = tokens.[21]                |> int
        let geodeRobotOreCost      = tokens.[27]                |> int
        let geodeRobotObsidianCost = tokens.[30]                |> int
        {
            Id                = id
            OreRobotCost      = oreRobotCost
            ClayRobotCost     = clayRobotCost
            ObsidianRobotCost = {OreCost = obsidianRobotOreCost; ClayCost     = obsidianRobotClayCost}
            GeodeRobotCost    = {OreCost = geodeRobotOreCost;    ObsidianCost = geodeRobotObsidianCost}
        }

    let display blueprint =
        printfn "Blueprint %d:" blueprint.Id
        printfn "  Each ore robot costs %d ore."                   blueprint.OreRobotCost
        printfn "  Each clay robot costs %d ore."                  blueprint.ClayRobotCost
        printfn "  Each obsidian robot costs %d ore and %d clay."  blueprint.ObsidianRobotCost.OreCost blueprint.ObsidianRobotCost.ClayCost
        printfn "  Each geode robot costs %d ore and %d obsidian." blueprint.GeodeRobotCost.OreCost    blueprint.GeodeRobotCost.ObsidianCost