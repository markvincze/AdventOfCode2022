open System
open System.IO
open System.Numerics

type Monkey =
    { Items: int64 list
      Operation: int64 -> int64
      DivisibleTest: int64
      TrueTarget: int
      FalseTarget: int
      Inspected: int64 }

let parseMonkey (lines: string list) =
    let [| _; numbers |] = (List.item 1 lines).Split ':'
    let numbers = numbers.Split(',', StringSplitOptions.TrimEntries) |> List.ofArray |> List.map Int64.Parse

    let [| _; operation |] = (List.item 2 lines).Split "= "
    // printfn "operation: %s" operation
    let [| _; op; op2 |] = operation.Split(' ', StringSplitOptions.TrimEntries)
    // printfn "op: %s, op2: %s" op op2
    let operation = match op with
                    | "*" -> match op2 with
                             | "old" -> (fun x -> x * x)
                             | op2 -> let op2 = Int64.Parse op2
                                      (fun x -> x * op2)
                    | "+" -> match op2 with
                             | "old" -> (fun x -> x + x)
                             | op2 -> let op2 = Int64.Parse op2
                                      (fun x -> x + op2)
    
    let divisibleTest = ((List.item 3 lines).Split "by ").[1] |> Int64.Parse
    let trueTarget = ((List.item 4 lines).Split "monkey ").[1] |> Int32.Parse
    let falseTarget = ((List.item 5 lines).Split "monkey ").[1] |> Int32.Parse
    let remainingLines = List.skip 6 lines |> List.skipWhile (fun l -> l = "")
    {
        Items = numbers
        Operation = operation
        DivisibleTest = divisibleTest
        TrueTarget = trueTarget
        FalseTarget = falseTarget
        Inspected = 0
    }, remainingLines

let rec parseMonkeys lines monkeys =
    match lines with 
    | [] -> List.rev monkeys
    | lines -> let monkey, newLines = parseMonkey lines
               parseMonkeys newLines (monkey :: monkeys)

let rec executeTurn divideWorry lcd index (monkeys: Monkey array) =
    let monkey = monkeys.[index]
    match monkey.Items with
    | [] -> monkeys
    | h :: t -> let worryLevel = if divideWorry then (monkey.Operation h) / 3L
                                 else ((monkey.Operation h) % lcd)
                let targetIndex = if worryLevel % monkey.DivisibleTest = 0
                                  then monkey.TrueTarget
                                  else monkey.FalseTarget
                let targetMonkey = monkeys.[targetIndex]
                monkeys.[targetIndex] <- { targetMonkey with Items = targetMonkey.Items @ [worryLevel] }
                monkeys.[index] <- { monkey with Items = t; Inspected = monkey.Inspected + 1L }
                executeTurn divideWorry lcd index monkeys

let executeRound divideWorry lcd monkeys =
    for i in 0..Array.length monkeys - 1 do
        executeTurn divideWorry lcd i monkeys |> ignore
    monkeys

let rec executeRounds divideWorry lcd n monkeys =
    if n % 500 = 0 then printfn "%d rounds left" n
    match n with
    | 0 -> monkeys
    | n -> let monkeys = executeRound divideWorry lcd monkeys
           executeRounds divideWorry lcd (n - 1) monkeys

let monkeys1 = parseMonkeys (File.ReadAllLines "FSharp/11-monkey-input.txt" |> List.ofArray) [] |> Array.ofList

let lcd = monkeys1 |> Array.map (fun m -> m.DivisibleTest) |> Array.reduce (*)

let endMonkeys1 = executeRounds true lcd 20 monkeys1
let result1 = endMonkeys1
              |> Array.sortByDescending (fun m -> m.Inspected)
              |> Array.take 2
              |> Array.map (fun m -> m.Inspected)
              |> Array.reduce (*)

let monkeys2 = parseMonkeys (File.ReadAllLines "FSharp/11-monkey-input.txt" |> List.ofArray) [] |> Array.ofList

let endMonkeys2 = executeRounds false lcd 10000 monkeys2
let result2 = endMonkeys2
              |> Array.sortByDescending (fun m -> m.Inspected)
              |> Array.take 2
              |> Array.map (fun m -> m.Inspected)
              |> Array.reduce (*)