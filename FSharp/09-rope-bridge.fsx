open System
open System.IO

type Dir =
| Up
| Right
| Down
| Left

let parseMotion (line: string) =
    let [| d; s |] = line.Split ' '
    let s = Int32.Parse s
    match d with
    | "U" -> Up, s
    | "R" -> Right, s
    | "D" -> Down, s
    | "L" -> Left, s
    | _ -> failwith "Invalid input"

let isTouching (x1, y1) (x2, y2) =
    (abs (x1 - x2)) <= 1 && (abs (y1 - y2)) <= 1

let move dir (x, y) =
    match dir with
    | Up -> (x, y - 1)
    | Right -> (x + 1, y)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)

let follow (tx, ty) (hx, hy) =
    if isTouching (tx, ty) (hx, hy)
    then (tx, ty)
    else let nx = tx + (sign (hx - tx))
         let ny = ty + (sign (hy - ty))
         (nx, ny)

let followTail tail hpos =
    List.scan
        (fun next current -> follow current next)
        hpos
        tail
    |> List.skip 1

let rec moveN dir n tail hpos visited =
    let visited = Set.add (List.last tail) visited
    if n = 0
    then tail, hpos, visited
    else let newHpos = move dir hpos
         moveN dir (n - 1) (followTail tail newHpos) newHpos visited

let rec processMotions motions tail hpos visited =
    match motions with
    | [] -> visited
    | (d, s) :: t -> let tpos, hpos, visited = moveN d s tail hpos visited
                     processMotions t tpos hpos visited

let motions = File.ReadAllLines "FSharp/09-rope-bridge-input.txt"
              |> List.ofArray
              |> List.map parseMotion

let visited1 = processMotions motions [(0, 0)] (0, 0) Set.empty<int * int>
let result1 = Set.count visited1

let visited2 = processMotions motions [for _ in 1..9 -> (0, 0)] (0, 0) Set.empty<int * int>
let result2 = Set.count visited2
