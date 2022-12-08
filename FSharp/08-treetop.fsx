open System
open System.IO

let lines = File.ReadAllLines "FSharp/08-treetop-input.txt"
let trees = Array2D.init
                (lines.Length)
                (lines.[0].Length)
                (fun x y -> lines.[x].[y] |> string |> Int32.Parse)

let isVisible (trees: int[,]) x y =
    let visibleLeft = [0..(x - 1)] |> List.forall (fun xr -> trees.[xr, y] < trees.[x, y])
    let visibleUp = [0..(y - 1)] |> List.forall (fun yr -> trees.[x, yr] < trees.[x, y])
    let visibleRight = [(x + 1)..((Array2D.length1 trees) - 1)] |> List.forall (fun xr -> trees.[xr, y] < trees.[x, y])
    let visibleDown = [(y + 1)..((Array2D.length2 trees) - 1)] |> List.forall (fun yr -> trees.[x, yr] < trees.[x, y])

    visibleLeft || visibleUp || visibleRight || visibleDown

[for x in 0..((Array2D.length1 trees) - 1) ->
    [for y in 0..((Array2D.length2 trees) - 1) -> ()]]

let result1 =
    seq { for y in 0..((Array2D.length1 trees) - 1) do
            for x in 0..((Array2D.length2 trees) - 1) do
                yield x, y }
    |> Seq.filter (fun (x, y) -> isVisible trees x y)
    |> Seq.length

let scenicScore (trees: int[,]) x y =
    let vdLeft = let shorterCount = [0..(x - 1)] |> List.rev |> List.takeWhile (fun xr -> trees.[xr, y] < trees.[x, y]) |> List.length
                 if shorterCount = x then shorterCount else shorterCount + 1
    let vdUp = let shorterCount = [0..(y - 1)] |> List.rev |> List.takeWhile (fun yr -> trees.[x, yr] < trees.[x, y]) |> List.length
               if shorterCount = y then shorterCount else shorterCount + 1
    let vdRight = let shorterCount = [(x + 1)..((Array2D.length1 trees) - 1)] |> List.takeWhile (fun xr -> trees.[xr, y] < trees.[x, y]) |> List.length
                  if shorterCount = (Array2D.length1 trees) - x - 1 then shorterCount else shorterCount + 1
    let vdDown = let shorterCount = [(y + 1)..((Array2D.length2 trees) - 1)] |> List.takeWhile (fun yr -> trees.[x, yr] < trees.[x, y]) |> List.length
                 if shorterCount = (Array2D.length2 trees) - y - 1 then shorterCount else shorterCount + 1

    vdLeft * vdUp * vdRight * vdDown

let s1 = scenicScore trees 0 0

let result2 =
    seq { for y in 0..((Array2D.length1 trees) - 1) do
            for x in 0..((Array2D.length2 trees) - 1) do
                yield x, y }
    |> Seq.map (fun (x, y) -> scenicScore trees x y)
    |> Seq.max