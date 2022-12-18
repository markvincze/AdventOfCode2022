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

let blockedRange row limits sensor =
    let (px, py) = sensor.Position
    let diff = sensor.Distance - (abs (row - py))
    if diff < 0
    then None
    else match limits with
         | None -> Some (px - diff, px + diff)
         | Some (low, high) -> if px + diff < low || px - diff > high
                               then None
                               else Some (max low (px - diff), min high (px + diff))

let blockedRanges sensors row limits =
    sensors
    |> List.choose (blockedRange row limits)

let countBlocked blockedRanges =
    let rec countBlocked blockedRanges coveredUntil acc =
        match blockedRanges with
        | [] -> acc
        | (x1, x2) :: t -> match coveredUntil with
                           | None -> countBlocked t (Some x2) (acc + x2 - x1 + 1)
                           | Some cy -> if x1 > cy then countBlocked t (Some x2) (acc + x2 - x1)
                                        else if x2 <= cy then countBlocked t (Some cy) acc
                                        else countBlocked t (Some x2) (acc + x2 - cy)

    countBlocked (blockedRanges |> List.sortBy fst) None 0

let row = 2000000

let result1 = blockedRanges sensors row None
              |> countBlocked

let highLimit = 4000000
let (missingBeaconRow, _) =
    [0..highLimit]
    |> List.map (fun r -> r, blockedRanges sensors r (Some (0, highLimit)) |> countBlocked)
    |> List.find (fun (r, c) -> c < highLimit)

let blockedRangesInMissingRow = blockedRanges sensors missingBeaconRow (Some (0, highLimit))
let missingBeaconX = [0..highLimit]
                     |> List.find (fun x -> List.forall (fun (x1, x2) -> x < x1 || x > x2) blockedRangesInMissingRow)

let result2 = (int64 missingBeaconX) * 4000000L + (int64 missingBeaconRow)
