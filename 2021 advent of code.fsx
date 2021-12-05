// https://adventofcode.com/2021
open System
open System.IO
open System.Text.RegularExpressions

let readAllText path =
    File.ReadAllText(path)

let split (separator: string) (str: string) = 
    str.Split(separator) |> List.ofArray

let isMatch (pattern: string) (str: string) = 
    Regex.IsMatch(str, pattern)

let contains (pattern: string) (str: string) = 
    str.Contains(pattern)

let replace (oldValue: string) (newValue: string) (str: string) = 
    str.Replace(oldValue, newValue)

let substr (start: int) (length: int) (str: string) = 
    str.Substring(start, length)
    
let substr2 (start: int) (str: string) = 
    str.Substring(start)

let join (str: char seq) = 
    String.Join("", str)

let startWith (prefix: string) (str: string) = 
    str.StartsWith(prefix)

let trim (str: string) = 
    str.Trim()

let toBase10 bits = 
    Convert.ToInt32(bits, 2)

let readAllLines path =
    File.ReadAllLines(path)

// Day 5 - Part one

let entry5 = readAllLines "entry5.txt"

let parse (line: string) =
    let [part1; part2] = line |> split "->"
    let [x;y] = part1 |> split ","
    let [x';y'] = part2 |> split ","
    int x,int y,int x',int y'

let vents = entry5 |> Array.map parse

let maxX = vents |> Array.collect (fun (x,_,x',_) -> [|x;x'|]) |> Array.max
let maxY = vents |> Array.collect (fun (_,y,_,y') -> [|y;y'|]) |> Array.max        

let diagram = Array2D.zeroCreate (maxY+1) (maxX+1)

let computePoints (diagram: int[,]) =
    diagram
        |> Seq.cast<int>
        |> Seq.filter (fun x -> x > 1)
        |> Seq.length

vents
    |> Array.fold (fun diagram (x,y,x',y') ->
            if x <> x' && y <> y' then diagram
            else
                let startX, startY = min x x', min y y'
                let stopX, stopY = max x x',  max y y'
                Array2D.init (maxY+1) (maxX+1) (fun column line ->
                    if column >= startY && column <= stopY && line >= startX && line <= stopX then
                        diagram[column, line] + 1
                    else diagram[column, line])) diagram
    |> computePoints

// Day 5 - Part two

let diagram' = Array2D.zeroCreate (maxY+1) (maxX+1)

vents
    |> Array.fold (fun diagram (x,y,x',y') ->
            let minX, minY = min x x', min y y'
            let maxX, maxY = max x x', max y y'
            let diagram' = Array2D.copy diagram
            match x - x', y - y' with
            | 0,_ ->  for cpt in [minY..maxY] do diagram'[cpt, x] <- diagram.[cpt, x] + 1
            | _, 0 -> for cpt in [minX..maxX] do diagram'[y, cpt] <- diagram.[y, cpt] + 1
            | deltaX, deltaY ->
                let incX = if deltaX > 0 then -1 else 1
                let incY = if deltaY > 0 then -1 else 1
                for cpt in [0..maxX-minX] do diagram'[y+cpt*incY, x+cpt*incX] <- diagram.[y+cpt*incY, x+cpt*incX] + 1
            diagram') diagram'
    |> computePoints

// Day 4 - Part one

type Board = (int * bool) list list

let entry4 = readAllLines "entry4.txt" |> List.ofArray

let numbers = entry4.[0] |> split "," |> List.map int

let boards = entry4
                |> List.skip 2
                |> List.windowed 5
                |> List.filter (List.contains "" >> not)
                |> List.map (fun board -> 
                                    board
                                        |> List.map (fun line -> 
                                                        split " " line
                                                            |> List.filter ((<>) "")
                                                            |> List.map (fun n -> int n, false)))

let isWinningBoard (board: Board) =
    [ for x in 0..4 do
        [0..4] |> List.forall (fun y -> board.[y].[x] |> snd)
        [0..4] |> List.forall (fun y -> board.[x].[y] |> snd) ] |> List.contains true

let computeScore (number: int) (board: Board) =
    board 
        |> List.collect id 
        |> List.filter (snd >> not) 
        |> List.sumBy fst
        |> (*) number

let rec play (numbers: int list) (boards: Board list) =
    match numbers with
    | number::rest ->
        let boards' = List.map (List.map (List.map (fun (n,m) -> n, n=number||m))) boards
        match List.filter isWinningBoard boards' |> List.tryHead with
        | Some winner  -> number, winner
        | _ -> play rest boards'
    | [] -> failwith "oups"
    
play numbers boards
    ||> computeScore

// Day 4 - Part two

let rec play' (numbers: int list) (boards: (int * bool) list list list) (lastWinner: int * (int * bool) list list) =
    match numbers with
    | number::rest ->
        let boards' = List.map (List.map (List.map (fun (n,m) -> n, n=number||m))) boards
        match List.filter isWinningBoard boards' |> List.tryHead with
        | Some winner  -> play' rest (boards' |> List.filter (isWinningBoard >> not)) (number, winner)
        | _ -> play' rest boards' lastWinner
    | [] -> lastWinner

play' numbers boards (0, [[]])
    ||> computeScore
    
// Day 3 - Part one

let entry3 = readAllLines "entry3.txt" |> List.ofArray

let isBitZero = (=)'0'
let isBitOne = (=)'1'

let countBitsAtPosition (numbers: string list) (position: int) =
    let nthBytes = numbers |> List.map (fun x -> x.[position]) 
    List.countBy isBitOne nthBytes, List.countBy isBitZero nthBytes

let gamaRate, epsilonRate =
    [0..entry3.[0].Length-1]
        |> List.map (countBitsAtPosition entry3)
        |> List.fold (fun (gamaRate, epsilonRate) (ones, zeros) ->
            if zeros > ones then $"{gamaRate}0", $"{epsilonRate}1"
            else $"{gamaRate}1", $"{epsilonRate}0") ("","")
            
(toBase10 gamaRate) * (toBase10 epsilonRate)

// Day 3 - Part two
     
let findOxygenGeneratorRating (numbers: string list) = 
    let rec findOxygenGeneratorRating' (numbers: string list) (position: int) = 
        if numbers.Length = 1 then numbers.[0]
        else 
            let ones, zeros = countBitsAtPosition numbers position
            let numbers' =
                    if zeros > ones then numbers |> List.filter (fun x -> isBitZero x.[position]) 
                    else numbers |> List.filter (fun x -> isBitOne x.[position])
            findOxygenGeneratorRating' numbers' (position+1)

    findOxygenGeneratorRating' numbers 0 

let findCO2ScrubberRating (numbers: string list) = 
    let rec findCO2ScrubberRating' (numbers: string list) (position: int) = 
        if numbers.Length = 1 then numbers.[0]
        else 
            let ones, zeros = countBitsAtPosition numbers position
            let numbers' =
                if zeros <= ones then numbers |> List.filter (fun x -> isBitZero x.[position])
                else numbers |> List.filter (fun x -> isBitOne x.[position])
            findCO2ScrubberRating' numbers' (position+1)

    findCO2ScrubberRating' numbers 0 

[ findOxygenGeneratorRating
  findCO2ScrubberRating ]
    |> List.map (fun x -> x entry3)
    |> List.map toBase10 |> List.fold (*) 0

// Day 2 - Part one

let entry2 = readAllLines "entry2.txt" 
                |> Array.map (split " ")
                |> List.ofArray

let horizon, depth =
    entry2
        |> List.fold (fun (horizon, depth) [instruction; value] -> 
                        match instruction with
                        | "forward" -> (horizon + int value, depth)
                        | "down" -> (horizon, depth +  int value)
                        | "up" -> (horizon, depth - int value)) (0,0)
horizon + depth

// Day 2 - Part two

let horizon', depth', _ =
    entry2
        |> List.fold (fun (horizon, depth, aim) [instruction; value] -> 
                        match instruction with
                        | "forward" -> (horizon + int value, depth + aim * int value, aim)
                        | "down" -> (horizon, depth, aim + int value)
                        | "up" -> (horizon, depth, aim - int value)) (0,0,0)
 
horizon' + depth'

// Day 1 - Part one

let entry1 = readAllLines "entry1.txt" 
                |> Array.map int
                |> List.ofArray

let countDepthMeasurementIncreases (data: int list) =
    [1..data.Length-1]
        |> List.map (fun x -> data.[x] - data.[x-1] )
        |> List.filter (fun x -> x > 0)
        |> List.length

entry1 |> countDepthMeasurementIncreases 

// Day 1 - Part two

let createThreeMeasurementSlidingWindow (data: int list) =
    [2..data.Length-1] 
        |> List.map (fun x -> data.[x] + data.[x-1] + data.[x-2])

entry1 
    |> createThreeMeasurementSlidingWindow
    |> countDepthMeasurementIncreases