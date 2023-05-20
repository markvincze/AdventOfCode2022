open System
open System.Text.RegularExpressions
open System.IO

type Blueprint = {
    Id: int
    OreRobotCost : int
    ClayRobotCost : int
    ObsidianRobotCost : int * int
    GeodeRobotCost : int * int
    MaxOreCost : int
    MaxClayCost : int
    MaxObsidianCost : int
}

let createBlueprint id oreRobotCost clayRobotCost obsidianRobotCost geodeRobotCost =
    {
        Id = id
        OreRobotCost = oreRobotCost
        ClayRobotCost = clayRobotCost
        ObsidianRobotCost = obsidianRobotCost
        GeodeRobotCost = geodeRobotCost
        MaxOreCost = [ oreRobotCost; clayRobotCost; obsidianRobotCost |> fst; geodeRobotCost |> fst ] |> List.max
        MaxClayCost = obsidianRobotCost |> snd
        MaxObsidianCost = geodeRobotCost |> snd
    }

let parseBlueprint line =
    let matches = Regex.Match(line, @"^Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.$")
    createBlueprint 
        (matches.Groups.[1].Value |> Int32.Parse)
        (matches.Groups.[2].Value |> Int32.Parse)
        (matches.Groups.[3].Value |> Int32.Parse)
        (matches.Groups.[4].Value |> Int32.Parse, matches.Groups.[5].Value |> Int32.Parse)
        (matches.Groups.[6].Value |> Int32.Parse, matches.Groups.[7].Value |> Int32.Parse)

let rec calcMaxGeodes blueprint timeLeft ore clay obsidian geodes oreRobots clayRobots obsidianRobots geodeRobots =
    // printfn "calcMaxGeodes, timeLeft: %d ore: %d clay: %d obsidan: %d geodes: %d" timeLeft ore clay obsidian geodes
    if timeLeft = 0
    then geodes
    else let timeLeft = timeLeft - 1
        //  if 
        // if ore >= fst blueprint.GeodeRobotCost && obsidian >= snd blueprint.GeodeRobotCost
        // then calcMaxGeodes blueprint timeLeft (ore - (fst blueprint.GeodeRobotCost) + oreRobots) (clay + clayRobots) (obsidian - (snd blueprint.GeodeRobotCost) + obsidianRobots) (geodes + geodeRobots) oreRobots clayRobots obsidianRobots (geodeRobots + 1) |> Some
         [
             if ore >= blueprint.OreRobotCost && ore < blueprint.MaxOreCost * (timeLeft - 1)
             then calcMaxGeodes blueprint timeLeft (ore - blueprint.OreRobotCost + oreRobots) (clay + clayRobots) (obsidian + obsidianRobots) (geodes + geodeRobots) (oreRobots + 1) clayRobots obsidianRobots geodeRobots |> Some
             else None;
             if ore >= blueprint.ClayRobotCost && clay < blueprint.MaxClayCost * (timeLeft - 1)
             then calcMaxGeodes blueprint timeLeft (ore - blueprint.ClayRobotCost + oreRobots) (clay + clayRobots) (obsidian + obsidianRobots) (geodes + geodeRobots) oreRobots (clayRobots + 1) obsidianRobots geodeRobots |> Some
             else None;
             if ore >= fst blueprint.ObsidianRobotCost && clay >= snd blueprint.ObsidianRobotCost && obsidian < blueprint.MaxObsidianCost * (timeLeft - 1)
             then calcMaxGeodes blueprint timeLeft (ore - (fst blueprint.ObsidianRobotCost) + oreRobots) (clay - (snd blueprint.ObsidianRobotCost) + clayRobots) (obsidian + obsidianRobots) (geodes + geodeRobots) oreRobots clayRobots (obsidianRobots + 1) geodeRobots |> Some
             else None;
             if ore >= fst blueprint.GeodeRobotCost && obsidian >= snd blueprint.GeodeRobotCost
             then calcMaxGeodes blueprint timeLeft (ore - (fst blueprint.GeodeRobotCost) + oreRobots) (clay + clayRobots) (obsidian - (snd blueprint.GeodeRobotCost) + obsidianRobots) (geodes + geodeRobots) oreRobots clayRobots obsidianRobots (geodeRobots + 1) |> Some
             else None;
             calcMaxGeodes blueprint timeLeft (ore + oreRobots) (clay + clayRobots) (obsidian + obsidianRobots) (geodes + geodeRobots) oreRobots clayRobots obsidianRobots geodeRobots |> Some
         ]
         |> List.filter Option.isSome
         |> List.map Option.get
         |> List.max

let blueprints = File.ReadAllLines "FSharp/19-minerals-input-short.txt" |> Array.map parseBlueprint |> List.ofArray

let maxGeodes =
    blueprints
    |> List.map (fun b -> (calcMaxGeodes b 24 0 0 0 0 1 0 0 0) * b.Id)
    |> List.sum
