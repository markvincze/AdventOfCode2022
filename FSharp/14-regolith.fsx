open System
open System.IO

type State =
| Air
| Rock
| Sand

type Path = (int * int) list

let parse (line: string) = line.Split " -> "
                           |> Array.map (fun p -> p.Split ',')
                           |> Array.map (fun [|x; y|] -> Int32.Parse x, Int32.Parse y)
                           |> List.ofArray

let rec markPath (state: State[,]) path =
    match path with
    | (x1, y1) :: ((x2, y2) :: r) ->
        if x1 = x2
        then [(min y1 y2)..(max y1 y2)] |> List.iter (fun y -> state[x1, y] <- Rock)
        else [(min x1 x2)..(max x1 x2)] |> List.iter (fun x -> state[x, y1] <- Rock)
        markPath state ((x2, y2) :: r)
    | _ -> state

let fallSand state =
    let rec fallSand (state: State[,]) (x, y) =
        if y >= (Array2D.length2 state) - 1
        then None
        else if state.[x, y + 1] = Air
        then fallSand state (x, y + 1)
        else if state.[x - 1, y + 1] = Air
        then fallSand state (x - 1, y + 1)
        else if state.[x + 1, y + 1] = Air
        then fallSand state (x + 1, y + 1)
        else Some (x, y)
    
    fallSand state (500, 0)

let rec progressUntilFull state =
    match fallSand state with
    | Some (500, 0) -> printfn "Stopping because rested at 500, 0"
                       state[500, 0] <- Sand
                       state
    | Some (x, y) -> state[x, y] <- Sand
                     progressUntilFull state
    | None -> printfn "Stopping because fell into the abyss"
              state

let finalState1 = File.ReadAllLines "FSharp/14-regolith-input.txt"
                  |> Array.map parse
                  |> List.ofArray
                  |> List.fold markPath (Array2D.create 1000 1000 Air)
                  |> progressUntilFull

let result1 = [ for x in 0..Array2D.length1 finalState1 - 1 do
                    for y in 0..Array2D.length2 finalState1 - 1 -> (x, y)]
              |> List.filter (fun (x, y) -> finalState1.[x, y] = Sand)
              |> List.length

let floorY = ([ for x in 0..Array2D.length1 finalState1 - 1 do
                   for y in 0..Array2D.length2 finalState1 - 1 -> (x, y)]
             |> List.filter (fun (x, y) -> finalState1.[x, y] = Rock)
             |> List.map snd
             |> List.max) + 2

let finalState2 = File.ReadAllLines "FSharp/14-regolith-input.txt"
                  |> Array.map parse
                  |> List.ofArray
                  |> List.append [[(0, floorY); (999, floorY)]]
                  |> List.fold markPath (Array2D.create 1000 1000 Air)
                  |> progressUntilFull

let result2 = [ for x in 0..Array2D.length1 finalState2 - 1 do
                    for y in 0..Array2D.length2 finalState2 - 1 -> (x, y)]
              |> List.filter (fun (x, y) -> finalState2.[x, y] = Sand)
              |> List.length

let print (state: State[,]) =
    for y in 0..14 do
        for x in 490..510 do
            match state.[x, y] with
            | Air -> printf "."
            | Rock -> printf "#"
            | Sand -> printf "o"
        printfn ""
    printfn ""
