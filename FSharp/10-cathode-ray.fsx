open System
open System.IO

type Op =
| Add of int
| Noop

let parse line =
    match line with
    | "noop" -> Noop
    | line -> Add (line.Split(' ').[1] |> Int32.Parse)

let calcSignalStrengthSum x cycle signalStrengthSum =
    // if cycle = 20 || (cycle % 40 = 0 && cycle <= 220)
    if (cycle + 20) % 40 = 0 && cycle <= 220
    then printfn "Adding to sum, cycle: %d, x: %d" cycle x
         signalStrengthSum + (x * cycle)
    else signalStrengthSum

let execute op x cycle signalStrengthSum =
    match op with
    | Noop -> x, (cycle + 1), (calcSignalStrengthSum x cycle signalStrengthSum)
    | Add y -> let signalStrengthSum = calcSignalStrengthSum x cycle signalStrengthSum
               x + y, (cycle + 2), (calcSignalStrengthSum x (cycle + 1) signalStrengthSum)

let ops = File.ReadAllLines "FSharp/10-cathode-ray-input.txt" |> List.ofArray |> List.map parse

let processProgram ops =
    let rec processProgram x cycle signalStrengthSum ops =
        match ops with
        | [] -> signalStrengthSum
        | h :: t -> let x, cycle, signalStrengthSum = execute h x cycle signalStrengthSum
                    processProgram x cycle signalStrengthSum t
    
    processProgram 1 1 0 ops

let result1 = processProgram ops