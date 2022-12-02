open System.IO
type Outcome =
| Win
| Loss
| Draw

type Choice =
| Rock
| Paper
| Scissors

let parseOpponentMove = function
                        | 'A' -> Rock
                        | 'B' -> Paper
                        | 'C' -> Scissors
                        | _ -> failwith "Invalid input"

let parseSelfMove = function
                    | 'X' -> Rock
                    | 'Y' -> Paper
                    | 'Z' -> Scissors
                    | _ -> failwith "Invalid input"

let outcome self opponent =
    match self, opponent with
    | Rock, Rock -> Draw
    | Rock, Paper -> Loss
    | Rock, Scissors -> Win
    | Paper, Rock -> Win
    | Paper, Paper -> Draw
    | Paper, Scissors -> Loss
    | Scissors, Rock -> Loss
    | Scissors, Paper -> Win
    | Scissors, Scissors -> Draw

let score self outcome =
    let moveScore = match self with
                    | Rock -> 1
                    | Paper -> 2
                    | Scissors -> 3

    let outcomeScore = match outcome with
                       | Win -> 6
                       | Loss -> 0
                       | Draw -> 3
    
    moveScore + outcomeScore

let process1 (line : string) =
    let [| opponentCode; _; selfCode |] = line.ToCharArray()
    let self, opponent = selfCode |> parseSelfMove, opponentCode |> parseOpponentMove
    let outcome = outcome self opponent
    score self outcome

let result1 = File.ReadAllLines "FSharp/02-rock-paper-scissors-input.txt"
              |> Array.map process1
              |> Array.sum

let parseOutcome = function
                   | 'X' -> Loss
                   | 'Y' -> Draw
                   | 'Z' -> Win

let determineSelfMove opponent outcome =
    match opponent, outcome with
    | Rock, Win -> Paper
    | Rock, Loss -> Scissors
    | Rock, Draw -> Rock
    | Paper, Win -> Scissors
    | Paper, Loss -> Rock
    | Paper, Draw -> Paper
    | Scissors, Win -> Rock
    | Scissors, Loss -> Paper
    | Scissors, Draw -> Scissors

let process2 (line : string) =
    let [| opponentCode; _; outcomeCode |] = line.ToCharArray()
    let outcome, opponent = outcomeCode |> parseOutcome, opponentCode |> parseOpponentMove
    let self = determineSelfMove opponent outcome
    score self outcome

let result2 = File.ReadAllLines "FSharp/02-rock-paper-scissors-input.txt"
              |> Array.map process2
              |> Array.sum
