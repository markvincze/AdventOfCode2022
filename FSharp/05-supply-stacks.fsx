open System
open System.Text.RegularExpressions
open System.IO

let push element stack = element :: stack
let pushn oneAtATime elements stack = List.append (if oneAtATime then (List.rev elements) else elements) stack
let pop stack = match stack with
                | h :: t -> h, t
                | [] -> failwith "pop: No elements"
let popn n stack =
    let rec popn stack n acc =
        match n with
        | 0 -> acc |> List.rev, stack
        | n -> match stack with
               | h :: t -> popn t (n - 1) (h :: acc)
               | [] -> failwith "popn: No elements"
    popn stack n []

type Move = {
    Quantity: int
    From: int
    To: int
}

let parseMove line =
    let rx = new Regex(@"move (\d*) from (\d*) to (\d*)")
    let m = rx.Match(line)
    {
        Quantity = m.Groups[1].Value |> Int32.Parse
        From = m.Groups[2].Value |> Int32.Parse
        To = m.Groups[3].Value |> Int32.Parse
    }

let m = parseMove "move 11 from 2 to 8"

let readStartingStacks (lines: string list) =
    let stacksLines = lines |> List.takeWhile (fun l -> l <> "")
    let l1, l2 = List.splitAt ((List.length stacksLines) - 1) stacksLines
    l1, List.exactlyOne l2

let (stacksLines, numsLine) = readStartingStacks (File.ReadAllLines "FSharp/05-supply-stacks-input.txt" |> List.ofArray)

let buildStacks (numsLine: string) stacksLines =
    let stackCount = (numsLine.Length + 1) / 4
    let rec buildStacks (stacksLines: string list) stacks =
        match stacksLines with
        | [] -> stacks
        | h :: t -> stacks
                    |> List.indexed
                    |> List.map (fun (i, s) -> match h.[i * 4 + 1] with
                                               | ' ' -> s
                                               | c -> push c s)
                    |> buildStacks t
    
    buildStacks (List.rev stacksLines) [ for _ in 1 .. stackCount -> []]

let stacks = buildStacks numsLine stacksLines

let executeMove oneAtATime stacks move =
    let toMove, remaining = popn move.Quantity (List.item (move.From - 1) stacks)

    [1 .. (List.length stacks)]
    |> List.map (fun i -> if i = move.From then remaining
                          else if i = move.To then pushn oneAtATime toMove (List.item (i - 1) stacks)
                          else List.item (i - 1) stacks)

let moves = File.ReadAllLines "FSharp/05-supply-stacks-input.txt"
            |> List.ofArray
            |> List.skipWhile (fun l -> l <> "")
            |> List.skip 1
            |> List.map parseMove

let finalState1 =
    moves
    |> List.fold (executeMove true) stacks

let result1 = finalState1
              |> List.filter (List.isEmpty >> not)
              |> List.map (List.head >> string)
              |> String.concat ""

let finalState2 =
    moves
    |> List.fold (executeMove false) stacks

let result2 = finalState2
              |> List.filter (List.isEmpty >> not)
              |> List.map (List.head >> string)
              |> String.concat ""