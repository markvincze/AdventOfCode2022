open System.Text.RegularExpressions
open System
open System.IO

type Sensor = {
    Position: int * int
    Beacon: int * int
    Distance: int
}

let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let parse line =
    let regex = new Regex("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)")
    let matches = (regex.Match line).Groups |> Seq.skip 1 |> Seq.map (fun g -> Int32.Parse g.Value) |> Array.ofSeq
    {
        Position = matches.[0], matches.[1]
        Beacon = matches.[2], matches.[3]
        Distance = dist (matches.[0], matches.[1]) (matches.[2], matches.[3])
    }

let sensors = File.ReadAllLines "FSharp/15-beacon-input.txt"
              |> List.ofArray
              |> List.map parse

let blockedRange row sensor =
    let diff = sensor.Distance - (abs (row - (snd sensor.Position)))
    if diff < 0
    then None
    else Some ((fst sensor.Position - diff), (fst sensor.Position + diff))

let blockedRanges sensors row =
    sensors
    |> List.choose (blockedRange row)

let countBlocked blockedRanges =
    let rec countBlocked blockedRanges coveredUntil acc =
        match blockedRanges with
        | [] -> acc
        | (x1, x2) :: t -> match coveredUntil with
                        | None -> countBlocked t (Some x2) (acc + x2 - x1)
                        | Some cy -> if x1 > cy then countBlocked t (Some x2) (acc + x2 - x1)
                                        else if x2 <= cy then countBlocked t (Some cy) acc
                                        else countBlocked t (Some x2) (acc + x2 - cy)

    countBlocked (blockedRanges |> List.sortBy fst) None 0

let row = 2000000

let result1 = blockedRanges sensors row
              |> countBlocked
