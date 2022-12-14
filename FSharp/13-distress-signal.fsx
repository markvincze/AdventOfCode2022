open System

type PacketData =
| Int of int
| Lst of PacketData list

let parseList line =
    let rec parseList (line: string) index acc =
        printfn "Called with index %d, acc: %A" index acc
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

// let lst = parseList "[1,1,3,1,1]"
let lst2 = parseList "[1,[2,[3,[4,[5,6,7]]]],8,9]"
