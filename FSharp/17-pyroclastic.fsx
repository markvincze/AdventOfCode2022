open System.IO

let coords shape (x, y) =
    match shape with
    | 0 -> [(x, y); (x + 1, y); (x + 2, y); (x + 3, y)]
    | 1 -> [(x, y + 1); (x + 1, y); (x + 1, y + 1); (x + 1, y + 2); (x + 2, y + 1)]
    | 2 -> [(x, y); (x + 1, y); (x + 2, y); (x + 2, y + 1); (x + 2, y + 2)]
    | 3 -> [(x, y); (x, y + 1); (x, y + 2); (x, y + 3)]
    | 4 -> [(x, y); (x + 1, y); (x, y + 1); (x + 1, y + 1)]
    | _ -> failwith "Invalid input"

let shapeHeight shape =
    match shape with
    | 0 -> 1
    | 1 -> 3
    | 2 -> 3
    | 3 -> 4
    | 4 -> 2
    | _ -> failwith "Invalid input"

type Dir = | Down | Left | Right

type Step = | Fall | Jet

let height minY chamber =
    seq { (max minY 0)..(Array2D.length2 chamber) - 1 }
    |> Seq.tryFind (fun y -> [0..6] |> List.forall (fun x -> not chamber.[x, y]))
    |> Option.defaultValue 0

let move dir (x, y) = match dir with
                      | Down -> (x, y - 1)
                      | Left -> (x - 1, y)
                      | Right -> (x + 1, y)

let collide (chamber: bool[,]) shape pos =
    coords shape pos
    |> List.exists (fun (x, y) -> x < 0 || x > 6 || y < 0 || chamber.[x, y])

let rec fallUntilStopped chamber (jets: string) jetIndex shape pos step =
    let dir = match step with
              | Fall -> Down
              | Jet -> if jets.[jetIndex] = '<' then Left else Right

    let newPos = move dir pos

    // if ((jetIndex) % jets.Length) = 0 then printfn "Reached end of jets in fallUntilStopped"

    match step, collide chamber shape newPos with
    | Fall, false -> fallUntilStopped chamber jets jetIndex shape newPos Jet
    | Fall, true -> pos, jetIndex
    | Jet, false -> fallUntilStopped chamber jets ((jetIndex + 1) % jets.Length) shape newPos Fall
    | Jet, true -> fallUntilStopped chamber jets ((jetIndex + 1) % jets.Length) shape pos Fall

let dropOne chamber jets jetIndex shape dropY =
    let dropPos = (2, dropY)

    let (stopPos, jetIndex) = fallUntilStopped chamber jets jetIndex shape dropPos Jet

    coords shape stopPos
    |> List.iter (fun (x, y) -> chamber.[x, y] <- true)

    jetIndex, snd stopPos

let mutable lastJet0Step = 0
let mutable lastJet0Height = 0

let rec dropN chamber (jets: string) jetIndex shape n ncnt dropY =
    if ((jetIndex + 1) % jets.Length) = 0
    then let h = (height (dropY - 5) chamber)
         printfn "Reached end of jets in dropN, n: %d, height: %d, shape: %d, step growth: %d, height growth: %d" ncnt h shape (ncnt - lastJet0Step) (h - lastJet0Height)
         lastJet0Step <- ncnt
         lastJet0Height <- h

    match n with
    | 0 -> chamber, dropY
    | n -> let jetIndex, stopY = dropOne chamber jets jetIndex shape dropY
           let nextDropY = (height stopY chamber) + 3
           dropN chamber jets jetIndex ((shape + 1) % 5) (n - 1) (ncnt + 1) nextDropY

let heightAfterN jets n =
    let afterN, finalDropY = dropN (Array2D.create 7 (n * 5) false) jets 0 0 n 0 3
    height (finalDropY - 5) afterN

let jets = File.ReadAllText "FSharp/17-pyroclastic-input.txt"

let result1 = heightAfterN jets 2022

// Reached end of jets in dropN, n: 1741, height: 2701, shape: 1, step growth: 1741, height growth: 2701
// Reached end of jets in dropN, n: 3476, height: 5396, shape: 1, step growth: 1735, height growth: 2695
// Reached end of jets in dropN, n: 5211, height: 8091, shape: 1, step growth: 1735, height growth: 2695
// Reached end of jets in dropN, n: 6946, height: 10786, shape: 1, step growth: 1735, height growth: 2695
// Reached end of jets in dropN, n: 8681, height: 13481, shape: 1, step growth: 1735, height growth: 2695

let rocksAfterInitialCycle = 1000000000000L - 1741L
let cycles = rocksAfterInitialCycle / 1735L
let extraRocks = rocksAfterInitialCycle % 1735L

let result2 = ((heightAfterN jets (1741 + (extraRocks |> int))) |> int64) + (cycles * 2695L)
