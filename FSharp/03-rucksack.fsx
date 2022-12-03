open System.IO

let lines = File.ReadAllLines "FSharp/03-rucksack-input.txt"

let commonElement (line : string) =
    let comp1 = line.Substring(0, line.Length / 2)
    let comp2 = line.Substring(line.Length / 2)

    Set.intersect (Set.ofSeq comp1) (Set.ofSeq comp2)
    |> Seq.exactlyOne

let priority c =
    if c >= 'a' && c <= 'z'
    then (int32 c) - (int32 'a') + 1
    else (int32 c) - (int32 'A') + 27

let result1 = lines
              |> Array.map commonElement
              |> Array.sumBy priority

let badge (lines: string array) =
    lines
    |> Array.map Set.ofSeq
    |> Set.intersectMany
    |> Seq.exactlyOne

let result2 = lines
              |> Array.chunkBySize 3
              |> Array.map badge
              |> Array.sumBy priority
