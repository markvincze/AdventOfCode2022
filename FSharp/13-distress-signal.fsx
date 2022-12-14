type PacketData =
| Int of int
| List of PacketData[]

let parseList line =
    let rec parseList (line: string) index acc =
        match line.[index + 1] with
        | '[' -> let list, index = parseList line (index + 1) []
                 parseList line (index + 1) (list :: acc)
        | ',' -> parseList line (index + 1) acc
        | ']' -> List (List.rev acc)
        | d -> 
