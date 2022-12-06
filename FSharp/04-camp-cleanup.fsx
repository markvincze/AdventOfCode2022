open System
open System.IO

let parse (line : string) =
    let [| r1; r2 |] = line.Split ','
    let [| a1; a2 |] = r1.Split '-' |> Array.map Int32.Parse
    let [| b1; b2 |] = r2.Split '-' |> Array.map Int32.Parse
    (a1, a2), (b1, b2)

let fullyContain ((a1, a2), (b1, b2)) =
    a1 <= b1 && a2 >= b2 ||
    b1 <= a1 && b2 >= a2

let result1 = File.ReadAllLines "FSharp/04-camp-cleanup-input.txt"
              |> Array.map parse
              |> Array.filter fullyContain
              |> Array.length

let overlap ((a1, a2), (b1, b2)) =
    not (b2 < a1 || b1 > a2)

let result2 = File.ReadAllLines "FSharp/04-camp-cleanup-input.txt"
              |> Array.map parse
              |> Array.filter overlap
              |> Array.length
