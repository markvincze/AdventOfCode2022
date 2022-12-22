open System.IO

let coords shape (x, y) =
    match shape with
    | 0 -> [(x, y); (x + 1, y); (x + 2, y); (x + 3, y)]
    | 1 -> [(x, y + 1); (x + 1, y); (x + 1, y + 1); (x + 1, y + 2); (x + 2, y + 1)]
    | 2 -> [(x, y); (x + 1, y); (x + 2, y); (x + 2, y + 1); (x + 2, y + 2)]
    | 3 -> [(x, y); (x, y + 1); (x, y + 2); (x, y + 3)]
    | 4 -> [(x, y); (x + 1, y); (x, y + 1); (x + 1, y + 1)]
    | _ -> failwith "Invalid input"

type Dir = | Down | Left | Right

type Step = | Fall | Jet

let height chamber =
    seq { 0..(Array2D.length2 chamber) - 1 }
    |> Seq.find (fun y -> [0..6] |> List.forall (fun x -> not chamber.[x, y]))

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

    match step, collide chamber shape newPos with
    | Fall, false -> fallUntilStopped chamber jets jetIndex shape newPos Jet
    | Fall, true -> pos, jetIndex
    | Jet, false -> fallUntilStopped chamber jets ((jetIndex + 1) % jets.Length) shape newPos Fall
    | Jet, true -> fallUntilStopped chamber jets ((jetIndex + 1) % jets.Length) shape pos Fall

let dropOne chamber jets jetIndex shape =
    let firstFreeRow = height chamber
    let dropPos = (2, (height chamber) + 3)

    let (stopPos, jetIndex) = fallUntilStopped chamber jets jetIndex shape dropPos Jet

    coords shape stopPos
    |> List.iter (fun (x, y) -> chamber.[x, y] <- true)

    jetIndex

let rec dropN chamber jets jetIndex shape n =
    match n with
    | 0 -> chamber
    | n -> let jetIndex = dropOne chamber jets jetIndex shape
           dropN chamber jets jetIndex ((shape + 1) % 5) (n - 1)

let chamber = Array2D.create 7 100000 false
let jets = File.ReadAllText "FSharp/17-pyroclastic-input.txt"

let after2022 = dropN chamber jets 0 0 2022
let result1 = height after2022

