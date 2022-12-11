open System
open System.IO
open System.Numerics

type Monkey =
    { Items: bigint list
      Operation: bigint -> bigint
      DivisibleTest: bigint
      TrueTarget: int
      FalseTarget: int
      Inspected: int64 }

let parseMonkey (lines: string list) =
    let [| _; numbers |] = (List.item 1 lines).Split ':'
    let numbers = numbers.Split(',', StringSplitOptions.TrimEntries) |> List.ofArray |> List.map BigInteger.Parse

    let [| _; operation |] = (List.item 2 lines).Split "= "
    // printfn "operation: %s" operation
    let [| _; op; op2 |] = operation.Split(' ', StringSplitOptions.TrimEntries)
    // printfn "op: %s, op2: %s" op op2
    let operation = match op with
                    | "*" -> match op2 with
                             | "old" -> (fun x -> x * x)
                             | op2 -> let op2 = BigInteger.Parse op2
                                      (fun x -> x * op2)
                    | "+" -> match op2 with
                             | "old" -> (fun x -> x + x)
                             | op2 -> let op2 = BigInteger.Parse op2
                                      (fun x -> x + op2)
    
    let divisibleTest = ((List.item 3 lines).Split "by ").[1] |> BigInteger.Parse
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

let monkeys1 = parseMonkeys (File.ReadAllLines "FSharp/11-monkey-input.txt" |> List.ofArray) [] |> Array.ofList

let rec executeTurn divideWorry index (monkeys: Monkey array) =
    let monkey = monkeys.[index]
    match monkey.Items with
    | [] -> monkeys
    | h :: t -> let worryLevel = if divideWorry then (monkey.Operation h) / bigint(3)
                                 else monkey.Operation h
                let targetIndex = if worryLevel % monkey.DivisibleTest = bigint(3)
                                  then monkey.TrueTarget
                                  else monkey.FalseTarget
                let targetMonkey = monkeys.[targetIndex]
                monkeys.[targetIndex] <- { targetMonkey with Items = targetMonkey.Items @ [worryLevel] }
                monkeys.[index] <- { monkey with Items = t; Inspected = monkey.Inspected + 1L }
                executeTurn divideWorry index monkeys

let executeRound divideWorry monkeys =
    for i in 0..Array.length monkeys - 1 do
        executeTurn divideWorry i monkeys |> ignore
    monkeys

let rec executeRounds divideWorry n monkeys =
    if n % 500 = 0 then printfn "%d rounds left" n
    match n with
    | 0 -> monkeys
    | n -> let monkeys = executeRound divideWorry monkeys
           executeRounds divideWorry (n - 1) monkeys

let endMonkeys1 = executeRounds true 20 monkeys1
let result1 = endMonkeys1
              |> Array.sortByDescending (fun m -> m.Inspected)
              |> Array.take 2
              |> Array.map (fun m -> m.Inspected)
              |> Array.reduce (*)

let monkeys2 = parseMonkeys (File.ReadAllLines "FSharp/11-monkey-input.txt" |> List.ofArray) [] |> Array.ofList

let endMonkeys2 = executeRounds false 100 monkeys2
let result2 = endMonkeys2
              |> Array.sortByDescending (fun m -> m.Inspected)
              |> Array.take 2
              |> Array.map (fun m -> m.Inspected)
              |> Array.reduce (*)