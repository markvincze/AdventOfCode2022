open System
open System.IO

type Node = {
    Parent: Node option
    mutable Children: Node list
    Name: string
    Size: int
    mutable TotalSize: int
}

type Command =
| Cd of string
| Ls

let parseCommand (line: string) =
    let split = line.Split ' '
    if split.[1] = "cd"
    then Cd (split.[2])
    else Ls

let parseDirEntry (line: string) =
    let [| size; name |] = line.Split ' '
    match size with
    | "dir" -> name, 0
    | size -> name, size |> Int32.Parse

let root = {
    Parent = None
    Children = []
    Name = "/"
    Size = 0
    TotalSize = 0
}

let rec processLsOutput (lines: string list) node =
    match lines with
    | [] -> []
    | h :: t -> if (h.[0] <> '$')
                then let name, size = parseDirEntry h
                     match (List.tryFind (fun c -> c.Name = name) node.Children) with
                     | Some c -> processLsOutput t node
                     | None -> let c = { Parent = Some node; Children = []; Name = name; Size = size; TotalSize = 0 }
                               node.Children <- (c :: node.Children)
                               processLsOutput t node
                else lines

let processCommands lines root =
    let rec processCommands lines root current =
        match lines with
        | [] -> root
        | h :: t -> match parseCommand h with
                    | Cd dir -> match dir with
                                | "/" -> processCommands t root root
                                | ".." -> match current.Parent with
                                          | Some parent -> processCommands t root parent
                                          | None -> failwith "Cannot go up further"
                                | d -> match List.tryFind (fun c -> c.Name = d) current.Children with
                                       | Some c -> processCommands t root c
                                       | None -> let c = { Parent = Some current; Children = []; Name = d; Size = 0; TotalSize = 0 }
                                                 current.Children <- (c :: current.Children)
                                                 processCommands t root c
                    | Ls -> let remaining = processLsOutput t current
                            processCommands remaining root current

    processCommands lines root root

let rec calcTotalSizes node =
    let childrenSize = node.Children |> List.sumBy calcTotalSizes
    let totalSize = node.Size + childrenSize
    node.TotalSize <- totalSize
    totalSize

let rec calcResult1 node =
    let childrenResult = node.Children |> List.sumBy calcResult1
    if node.Size = 0 && node.TotalSize <= 100000
    then childrenResult + node.TotalSize
    else childrenResult

let lines = File.ReadAllLines "FSharp/07-no-space-input.txt" |> List.ofArray

let tree = processCommands lines root

calcTotalSizes tree
let result1 = calcResult1 tree

let spaceNeeded = 30000000 - (70000000 - tree.TotalSize)
let rec findBestDirToDelete node =
    let fromChildren = List.choose findBestDirToDelete node.Children
    let bestFromChildren = match fromChildren with
                           | [] -> None
                           | l -> Some (List.min l)

    if node.Size = 0 && node.TotalSize >= spaceNeeded
    then match bestFromChildren with
         | None -> Some node.TotalSize
         | Some b -> if b < node.TotalSize then Some b else Some node.TotalSize 
    else bestFromChildren

let result2 = findBestDirToDelete tree
