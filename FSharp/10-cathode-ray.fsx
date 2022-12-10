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
    if (cycle + 20) % 40 = 0 && cycle <= 220
    then signalStrengthSum + (x * cycle)
    else signalStrengthSum

let drawPixel x cycle =
    let drawPos = (cycle - 1) % 40
    let linebreak = if drawPos = 39 then "\n" else ""
    if abs (x - drawPos) <= 1
    then printf "#%s" linebreak
    else printf ".%s" linebreak

let execute op x cycle signalStrengthSum =
    match op with
    | Noop -> drawPixel x cycle
              x, (cycle + 1), (calcSignalStrengthSum x cycle signalStrengthSum)
    | Add y -> drawPixel x cycle
               drawPixel x (cycle + 1)
               let signalStrengthSum = calcSignalStrengthSum x cycle signalStrengthSum
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
