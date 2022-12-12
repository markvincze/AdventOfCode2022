open System.IO

type Square =
    { Pos: int * int
      Elevation: char
      BestDist: int option
      Solved: bool }

let lines = File.ReadAllLines "FSharp/12-hill-climbing-input.txt"
let terrain = Array2D.init
                  (lines.[0]).Length
                  lines.Length
                  (fun x y -> { Pos = (x, y); BestDist = None; Elevation = (lines.[y].[x]); Solved = false })

let startPos = [ for x in 0..Array2D.length1 terrain - 1 do
                     for y in 0..Array2D.length2 terrain - 1 -> (x, y)]
               |> List.find (fun (x, y) -> terrain.[x, y].Elevation = 'S')

let endPos = [ for x in 0..Array2D.length1 terrain - 1 do
                   for y in 0..Array2D.length2 terrain - 1 -> (x, y)]
             |> List.find (fun (x, y) -> terrain.[x, y].Elevation = 'E')

terrain.[fst startPos, snd startPos] <- { terrain.[fst startPos, snd startPos] with Elevation = 'a'; BestDist = Some 0; Solved = true }
terrain.[fst endPos, snd endPos] <- { terrain.[fst endPos, snd endPos] with Elevation = 'z' }

let queue = [ startPos ]

let accessibleNeighbors (terrain: Square[,]) pos =
    let x, y = pos
    let s = terrain.[x, y]
    seq {
        if x > 0 && (int)terrain.[x - 1, y].Elevation <= (int)s.Elevation + 1 then yield terrain.[x - 1, y]
        if x < (Array2D.length1 terrain - 1) && (int)terrain.[x + 1, y].Elevation <= (int)s.Elevation + 1 then yield terrain.[x + 1, y]
        if y > 0 && (int)terrain.[x, y - 1].Elevation <= (int)s.Elevation + 1 then yield terrain.[x, y - 1]
        if y < (Array2D.length2 terrain - 1) && (int)terrain.[x, y + 1].Elevation <= (int)s.Elevation + 1 then yield terrain.[x, y + 1]
    }

let rec findShortestPath endPos queue (terrain: Square[,]) =
    // printfn "Queue: %A" queue
    let next :: queue = queue |> List.sortBy (fun p -> terrain.[fst p, snd p].BestDist.Value) |> List.distinct
    let square = terrain.[fst next, snd next]
    terrain.[fst next, snd next] <- { square with Solved = true }
    let ns = accessibleNeighbors terrain next
             |> Seq.filter (fun n -> not n.Solved)
    // printfn "ns: %A" ns
    for n in ns do
        match n.BestDist with
        | None -> terrain.[fst n.Pos, snd n.Pos] <- { n with BestDist = Some (square.BestDist.Value + 1) }
        | Some d -> if square.BestDist.Value + 1 < d
                        then terrain.[fst n.Pos, snd n.Pos] <- { n with BestDist = Some (square.BestDist.Value + 1) }
    let queue = queue
                |> List.append (ns |> Seq.map (fun n -> n.Pos) |> List.ofSeq)
    // printfn "Next queue: %A" queue
    // printfn "End square: %A" terrain.[fst endPos, snd endPos]
    if terrain.[fst endPos, snd endPos].Solved
    then terrain.[fst endPos, snd endPos].BestDist.Value
    else findShortestPath endPos queue terrain

let result1 = findShortestPath endPos queue terrain
