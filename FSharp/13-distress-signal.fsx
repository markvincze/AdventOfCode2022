open System
open System.IO

type PacketData =
| Int of int
| Lst of PacketData list

let parseList line =
    let rec parseList (line: string) index acc =
        match line.[index + 1] with
        | '[' -> let list, index = parseList line (index + 1) []
                 parseList line (index + 1) (list :: acc)
        | ',' -> parseList line (index + 1) acc
        | ']' -> Lst (List.rev acc), index
        | d -> let length = line |> Seq.skip (index + 1) |> Seq.findIndex (fun c -> Char.IsDigit(c) |> not)
               let num = line.Substring(index + 1, length) |> Int32.Parse
               parseList line (index + length) ((Int num) :: acc)
    
    let result, _ = parseList line 0 []
    result

type CompareResult = Correct | Incorrect | Equal

let rec compare a b =
    match a, b with
    | Int a, Int b -> if a < b then Correct else if a = b then Equal else Incorrect
    | Lst _, Int _ -> compare a (Lst [b])
    | Int _, Lst _ -> compare (Lst [a]) b
    | Lst (ha :: ta), Lst (hb :: tb) -> match compare ha hb with
                                        | Correct -> Correct
                                        | Incorrect -> Incorrect
                                        | Equal -> compare (Lst ta) (Lst tb)
    | Lst [], Lst (_ :: _) -> Correct
    | Lst (_ :: _), Lst [] -> Incorrect
    | Lst [], Lst [] -> Equal

let parsePair (lines: string list) =
    let l1 :: (l2 :: _) = lines
    parseList l1, parseList l2

let parsePairs lines =
    lines
    |> List.chunkBySize 3
    |> List.map parsePair

let result1 = File.ReadAllLines "FSharp/13-distress-signal-input.txt"
              |> List.ofArray
              |> parsePairs
              |> List.indexed
              |> List.filter (fun (_, (p1, p2)) -> compare p1 p2 = Correct )
              |> List.map fst
              |> List.map (fun i -> i + 1)
              |> List.sum

let sorted = File.ReadAllLines "FSharp/13-distress-signal-input.txt"
             |> List.ofArray
             |> parsePairs
             |> List.collect (fun (a, b) -> [a; b])
             |> List.append [Lst [ Lst [ Int 2 ]]; Lst [ Lst [ Int 6 ]]]
             |> List.sortWith (fun a b -> match (compare a b) with
                                          | Correct -> -1
                                          | Equal -> 0
                                          | Incorrect -> 1)

let result2 = ((sorted |> List.findIndex (fun p -> p = Lst [ Lst [ Int 2 ]])) + 1) *
              ((sorted |> List.findIndex (fun p -> p = Lst [ Lst [ Int 6 ]])) + 1)
