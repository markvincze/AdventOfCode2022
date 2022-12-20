open System.Text.RegularExpressions
open System
open System.IO

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

type Valve = {
    Label: string
    Rate: int
    Tunnels: Map<string, int>
}

let parse line =
    // Valve BB has flow rate=13; tunnels lead to valves CC, AA
    let regex = new Regex(@"Valve (\w\w) has flow rate=(\d+); tunnels? leads? to valves? (.*)")
    let matches = regex.Match line
    let tunnels = matches.Groups.[3].Value.Split ", "
    {
        Label = matches.Groups.[1].Value
        Rate = matches.Groups.[2].Value |> Int32.Parse
        Tunnels = tunnels |> List.ofArray |> List.map (fun t -> t, 1) |> Map.ofList
    }

let valves = File.ReadAllLines "FSharp/16-volcanium-input.txt"
             |> Array.map parse
             |> Array.map (fun v -> v.Label, v)
             |> Map.ofArray

let findShortestPathLength valves start target = 
    let rec findShortestPathLength valves target queue visited = 
        match queue with
        | [] -> failwith "No path"
        | (h, dist) :: t -> if Set.contains h visited then findShortestPathLength valves target t visited
                            else if h = target then dist
                            else let valve = Map.find h valves
                                 let queue =
                                    (valve.Tunnels |> Map.keys |> Seq.map (fun t -> (t, dist + 1)) |> List.ofSeq)
                                    |> List.append queue
                                 findShortestPathLength valves target queue (Set.add h visited)
    findShortestPathLength valves target [(start, 0)] Set.empty<string>

let buildSimplifiedMap valves = 
    let valvesWithRate = valves |> Map.filter (fun _ v -> v.Label = "AA" || v.Rate > 0) |> Map.values |> List.ofSeq
    valvesWithRate
    |> List.map (fun v ->
        let tunnels = valvesWithRate
                      |> List.filter (fun v' -> v'.Label <> v.Label)
                      |> List.map (fun v' -> v'.Label, findShortestPathLength valves v.Label v'.Label)
                      |> Map.ofList
        v.Label, { v with Tunnels = tunnels })
    |> Map.ofList

let rec findBest valves valveLabel opened minLeft =
    let valve = Map.find valveLabel valves
    if minLeft <= 1
    then 0
    else let gainedFlow = valve.Rate * (minLeft - 1)
         let results = valve.Tunnels
                       |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
                       |> List.ofSeq
                       |> List.filter (fun (v, _) -> Set.contains v opened |> not)
                       |> List.filter (fun (_, d) -> d < minLeft)
                       |> List.map (fun (t, d) ->
                          findBest valves t (Set.add valveLabel opened) (minLeft - d - (if gainedFlow > 0 then 1 else 0)))
         let bestSubResult = if List.isEmpty results then 0 else List.max results
         bestSubResult + gainedFlow

let simplifiedValves = (buildSimplifiedMap valves)

// let rec findBest2 valves valveLabel opened minLeft acc =
//     let valve = Map.find valveLabel valves
//     if minLeft <= 1
//     then acc
//     else let targetsLeft =
//              valves
//              |> Map.filter (fun k v -> v.Label <> valveLabel && v.Rate > 0 && (Set.contains k opened |> not))
//              |> Map.values
//          if Seq.isEmpty targetsLeft
//          then acc
//          else let nextTarget = targetsLeft
//                                |> Seq.maxBy (fun v -> (minLeft - valve.Tunnels.[v.Label] - 1) * v.Rate)
//               let gain = (minLeft - valve.Tunnels.[nextTarget.Label] - 1) * nextTarget.Rate
//               findBest2 valves nextTarget.Label (Set.add valveLabel opened) (minLeft - valve.Tunnels.[nextTarget.Label] - 1) (acc + gain)

let result1 = findBest simplifiedValves "AA" Set.empty<string> 30
// let result1_2 = findBest2 simplifiedValves "AA" Set.empty<string> 30 0


let rec findBest2 valves opened target1 target2 minLeft1 minLeft2 totalMinLeft =
    if totalMinLeft <= 1
    then 0
    else let target1, target2, minLeft1, minLeft2 =
            if minLeft1 <= minLeft2
            then target1, target2, minLeft1, minLeft2
            else target2, target1, minLeft2, minLeft1
         let valve = Map.find target1 valves
         let gainedFlow = valve.Rate * (totalMinLeft - minLeft1)
         let results =
             valve.Tunnels
             |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
             |> List.ofSeq
             |> List.filter (fun (v, _) -> v <> target2 && v <> "AA" && (Set.contains v opened |> not))
             |> List.filter (fun (_, d) -> d < totalMinLeft)
             |> List.map (fun (t, d) ->
                 findBest2 valves (Set.add target1 opened) t target2 (d + 1) (minLeft2 - minLeft1) (totalMinLeft - minLeft1))
         let bestSubResult = if List.isEmpty results
                             then valves.[target2].Rate * (totalMinLeft - minLeft2)
                             else List.max results
         bestSubResult + gainedFlow

let result2 = findBest2 simplifiedValves Set.empty<string> "AA" "AA" 0 0 26

