open System
open System.IO

type Pos = int * int * int
type Dir = X | Y | Z

type SideSpec = {
    Pos : Pos
    Dir : Dir
}

let rec processLines sides (lines : string list) =
    let addSide side sides =
        match Map.tryFind side sides with
        | None -> Map.add side 1 sides
        | Some c -> Map.add side (c + 1) sides

    match lines with
    | [] -> sides
    | h :: t -> let [| x; y; z |] = h.Split ',' |> Array.map Int32.Parse
                let sides = sides
                            |> addSide { Pos = x, y, z; Dir = X}
                            |> addSide { Pos = x, y, z; Dir = Y}
                            |> addSide { Pos = x, y, z; Dir = Z}
                            |> addSide { Pos = x - 1, y, z; Dir = X}
                            |> addSide { Pos = x, y - 1, z; Dir = Y}
                            |> addSide { Pos = x, y, z - 1; Dir = Z}
                processLines sides t

let sides = File.ReadAllLines "FSharp/18-boiling-boulders-input.txt"
            |> List.ofArray
            |> processLines Map.empty<SideSpec, int>

let result1 = sides
              |> Map.filter (fun _ c -> c = 1)
              |> Seq.length

// Part 2

let neighbors (x, y, z) =
    [
        x + 1, y, z
        x, y + 1, z
        x, y, z + 1
        x - 1, y, z
        x, y - 1, z
        x, y, z - 1
    ]

let inBounds minX minY minZ maxX maxY maxZ (x, y, z) =
    x >= minX && x <= maxX &&
    y >= minY && y <= maxY &&
    z >= minZ && z <= maxZ

let rec countSurface queue visited cubes surfaceCount minX minY minZ maxX maxY maxZ =
    match queue with
    | [] -> surfaceCount
    | h :: t -> if Set.contains h visited
                then countSurface t visited cubes surfaceCount minX minY minZ maxX maxY maxZ
                else let ns = neighbors h |> List.filter (inBounds minX minY minZ maxX maxY maxZ)
                     let queue = ns
                                 |> List.filter (fun n -> Set.contains n cubes |> not)
                                 |> List.filter (fun n -> Set.contains n visited |> not)
                                 |> List.append t
                     let surfaceCubes = ns
                                     |> List.filter (fun n -> Set.contains n cubes)
 
                     countSurface queue (Set.add h visited) cubes (surfaceCount + (List.length surfaceCubes)) minX minY minZ maxX maxY maxZ

let rec collectCubes cubes (lines : string list) =
    match lines with
    | [] -> cubes
    | h :: t -> let [| x; y; z |] = h.Split ',' |> Array.map Int32.Parse
                collectCubes (Set.add (x, y, z) cubes) t

let cubes = File.ReadAllLines "FSharp/18-boiling-boulders-input.txt"
            |> List.ofArray
            |> collectCubes Set.empty<Pos>

let minX = (cubes |> Seq.map (fun (x, _, _) -> x) |> Seq.min) - 1
let minY = (cubes |> Seq.map (fun (_, y, _) -> y) |> Seq.min) - 1
let minZ = (cubes |> Seq.map (fun (_, _, z) -> z) |> Seq.min) - 1
let maxX = (cubes |> Seq.map (fun (x, _, _) -> x) |> Seq.max) + 1
let maxY = (cubes |> Seq.map (fun (_, y, _) -> y) |> Seq.max) + 1
let maxZ = (cubes |> Seq.map (fun (_, _, z) -> z) |> Seq.max) + 1

let result2 = countSurface [(minX, minY, minZ)] Set.empty<Pos> cubes 0 minX minY minZ maxX maxY maxZ