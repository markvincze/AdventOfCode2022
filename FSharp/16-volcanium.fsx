open System.Text.RegularExpressions
open System
open System.IO

// Alg:
// Compare these:
// If opened already:
//  - Permute links in all possible orders, 
//  - For all permutations:
//    - Recursively call the method in every permuted order and sum the results
//    - 

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

type Valve = {
    Label: string
    Rate: int
    IsOpen: bool
    Tunnels: (string * int) list
}

// Valve BB has flow rate=13; tunnels lead to valves CC, AA
let parse line =
    let regex = new Regex(@"Valve (\w\w) has flow rate=(\d+); tunnels? leads? to valves? (.*)")
    let matches = regex.Match line
    let tunnels = matches.Groups.[3].Value.Split ", "
    {
        Label = matches.Groups.[1].Value
        Rate = matches.Groups.[2].Value |> Int32.Parse
        IsOpen = false
        Tunnels = tunnels |> List.ofArray |> List.map (fun t -> t, 1)
    }

let valves = File.ReadAllLines "FSharp/16-volcanium-input.txt"
             |> Array.map parse
             |> Array.map (fun v -> v.Label, v)
             |> Map.ofArray

// let startValve = Map.find "AA" valves

// type Step =
// | Move of string
// | Open

let findShortestPathLength valves start target = 
    let rec findShortestPathLength valves target queue visited = 
        match queue with
        | [] -> failwith "No path"
        | (h, dist) :: t -> if Set.contains h visited then findShortestPathLength valves target t visited
                            else if h = target then dist
                            else let valve = Map.find h valves
                                 let queue =
                                    (valve.Tunnels |> List.map (fun (t, _) -> (t, dist + 1)))
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
        v.Label, { v with Tunnels = tunnels })
    |> Map.ofList

let rec findBest valves valveLabel visitedWithoutOpen minLeft =
    if valves |> Map.forall (fun _ v -> v.IsOpen)
    then 0
    else let valve = Map.find valveLabel valves
         if minLeft <= 1
         then 0
         else let moveResults = valve.Tunnels
                                |> List.filter (fun (v, _) -> Set.contains v visitedWithoutOpen |> not)
                                // |> List.except visitedWithoutOpen
                                |> List.map (fun (t, d) -> findBest valves t (Set.add valveLabel visitedWithoutOpen) (minLeft - d))
              let openAndMoveResults =
                  if valve.Rate = 0 || valve.IsOpen
                  then []
                  else let gainedFlow = valve.Rate * (minLeft - 1)
                       let newValves = Map.add valve.Label { valve with IsOpen = true } valves
                       valve.Tunnels
                       |> List.map (fun (t, d) -> (findBest newValves t Set.empty<string> (minLeft - 1 - d)) + gainedFlow)
              let results = moveResults |> List.append openAndMoveResults
              let result = if List.isEmpty results then 0 else List.max results
            //   let result = moveResults
            //                |> List.append openAndMoveResults
            //                |> List.max
              if minLeft > 20 then printfn "Valve %s, MinLeft: %d, Result: %d" valve.Label minLeft result
              result

let simplifiedValves = (buildSimplifiedMap valves)

// let result1 = findBest simplifiedValves "AA" Set.empty<string> 30
