open System
open System.IO

let parseRations lines = 
    let rec parseRations lines rations nextRation =
        match lines with
        | "" :: t -> parseRations t (nextRation :: rations) []
        | h :: t -> parseRations t rations ((h |> Int32.Parse) :: nextRation)
        | [] -> nextRation :: rations
    parseRations lines [] []

let rations = File.ReadAllLines "FSharp/01-calorie-counting-input.txt"
              |> List.ofArray
              |> parseRations

let result1 = rations
              |> List.map List.sum
              |> List.max

let result2 = rations
              |> List.map List.sum
              |> List.sortDescending
              |> List.take 3
              |> List.sum