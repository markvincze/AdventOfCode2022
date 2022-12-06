open System.IO

let findMarker len (str: string) =
    let rec findStarter len (lastn: string) pos (str: string) =
        if lastn |> Seq.distinct |> Seq.length = len
        then pos
        else findStarter len (lastn.Substring(1) + str.Substring(0, 1)) (pos + 1) (str.Substring(1))

    findStarter len (str.Substring(0, len)) len (str.Substring(len))

let result1 = findMarker 4 (File.ReadAllText "FSharp/06-tuning-trouble-input.txt")

let result2 = findMarker 14 (File.ReadAllText "FSharp/06-tuning-trouble-input.txt")
