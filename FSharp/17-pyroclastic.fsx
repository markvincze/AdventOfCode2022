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

let rec dropN chamber (jets: string) jetIndex shape n dropY =
    if ((jetIndex + 1) % jets.Length) = 0
        then printfn "Reached end of jets in dropN, n: %d, height: %d, shape: %d" n (height (dropY - 5) chamber) shape

    match n with
    | 0 -> chamber, dropY
    | n -> let jetIndex, stopY = dropOne chamber jets jetIndex shape dropY
           let nextDropY = (height stopY chamber) + 3
           dropN chamber jets jetIndex ((shape + 1) % 5) (n - 1) nextDropY

let chamber = Array2D.create 7 100000 false
let jets = File.ReadAllText "FSharp/17-pyroclastic-input.txt"

// let after2022, finalDropY = dropN chamber jets 0 0 2022 3
// let result1 = height (finalDropY - 5) after2022

// let after10000, finalDropY2 = dropN chamber jets 0 0 10000 3
// let result2 = height (finalDropY2 - 5) after10000

let after140, finalDropY2 = dropN (Array2D.create 7 100000 false) jets 0 0 140 3
let heightAfter140 = height (finalDropY2 - 5) after140

let after1735, finalDropY1735 = dropN (Array2D.create 7 100000 false) jets 0 0 1735 3
let heightAfter1735 = height (finalDropY1735 - 5) after1735

let after1875, finalDropY1875 = dropN (Array2D.create 7 100000 false) jets 0 0 1875 3
let heightAfter1875 = height (finalDropY1875 - 5) after1875

let diff = heightAfter1875 - heightAfter1735

let stepCycle = 1735L
let growthPerCycle = 2695L
let totalDrops = 1000000000000L
let cycles = totalDrops / stepCycle
let dropsLeft = totalDrops % stepCycle

let fullh = cycles * growthPerCycle + (int64 (heightAfter1875 - heightAfter1735))