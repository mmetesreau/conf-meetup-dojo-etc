// https://adventofcode.com/2021
open System
open System.IO
open System.Collections.Generic

let memoization f =
    let cache = Dictionary<_,_>()
    fun c ->
        let exist, value = cache.TryGetValue (c)
        match exist with
        | true -> value
        | _ -> 
            let value = f c
            cache.Add (c, value)
            value

let split (separator: string) (str: string) = 
    str.Split(separator) |> List.ofArray

let readAllLines path =
    File.ReadAllLines(path) |> List.ofArray

// Day 8 - Part one

let entry8 = 
    readAllLines "Day.txt" 
        |> List.map (split "|" >> List.map (split " " >> List.filter ((<>) "") >> (List.map Set.ofSeq))) 

entry8
    |> List.collect (fun [_;outputs] -> outputs)
    |> List.filter (fun outputs -> outputs.Count = 2 || outputs.Count = 3|| outputs.Count = 4|| outputs.Count = 7)
    |> List.length

// Day 8 - Part two

let deducts ([signals;outputs]: Set<char> list list) =
    let map = 
        signals
            |> List.sortBy (fun x -> x.Count)
            |> List.fold (fun map  digit ->
                match digit.Count with
                | 2 -> Map.add digit "1" map
                | 3 -> Map.add digit "7" map
                | 4 -> Map.add digit "4" map
                | 5 -> 
                    let one = map |> Map.findKey (fun _ value -> value = "1") 
                    let four = map |> Map.findKey (fun _ value -> value = "4") 
                    if one |> Set.intersect digit |> Set.count = 2 then Map.add digit "3" map
                    elif four |> Set.intersect digit |> Set.count = 2 then Map.add digit "2" map
                    else Map.add digit "5" map
                | 6 -> 
                    let one = map |> Map.findKey (fun _ value -> value = "1") 
                    let four = map |> Map.findKey (fun _ value -> value = "4") 
                    if one |> Set.intersect digit |> Set.count = 1 then Map.add digit "6" map
                    elif four |> Set.intersect digit |> Set.count = 4 then Map.add digit "9" map
                    else Map.add digit "0" map
                | 7 -> Map.add digit "8" map
                | _ -> map) Map.empty
    outputs
        |> List.map (fun x -> map |> Map.find (x |> Set.ofSeq))

entry8
    |> List.map deducts
    |> List.sumBy (fun x -> String.Join("", x) |> int)

// Day 7 - Part one

let entry7 = readAllLines "entry7.txt"

let crabPositions = entry7.[0] |> split "," |> List.map int

let computeFuelConsumption (targetPosition: int) (position: int) = abs (position - targetPosition)

[
    for targetPosition in crabPositions do
        crabPositions |> List.sumBy (computeFuelConsumption targetPosition)
] |> List.min

// or 

crabPositions
    |> List.fold (fun minFuel targetPosition -> (crabPositions |> List.sumBy (computeFuelConsumption targetPosition), minFuel) ||> min) (Int32.MaxValue)

// Day 7 - Part two

let computeFuelConsumption' (targetPosition: int) (position: int) = [1.. abs (position - targetPosition)] |> List.sum
let computeFuelConsumption'M = memoization computeFuelConsumption'

[
    for targetPosition in [List.min crabPositions..List.max crabPositions] do
        crabPositions |> List.sumBy (computeFuelConsumption'M targetPosition)
] |> List.min

// or 

[List.min crabPositions..List.max crabPositions]
    |> List.fold (fun minFuel targetPosition -> (crabPositions |> List.sumBy (computeFuelConsumption'M targetPosition), minFuel) ||> min) (Int32.MaxValue)

// Day 6 - Part one

let entry6 = readAllLines "entry6.txt"

let timers = entry6.[0] |> split "," |> List.map int

[1..80]
    |> List.fold (fun timers _ ->
        [
            for timer in timers do
                let timer' = timer - 1
                if timer' < 0 then yield! [6;8]
                else yield timer' 
        ]) timers |> List.length

// Day 6 - Part two

let timers' = 
    timers
        |> List.groupBy id
        |> List.map (fun (x, xs) -> int64 x, int64 xs.Length)
        |> Map.ofList

[1..256]
    |> List.fold (fun timers _ ->
        [
            for timer,nbr in Map.toSeq timers do
                let timer' = timer - 1L
                if timer' < 0L then yield! [6L,nbr; 8L,nbr]
                else yield timer',nbr
        ]
            |> List.groupBy fst
            |> List.map (fun (timer, data) -> timer, List.sumBy snd data)
            |> Map.ofList) timers'
    |> Map.toList
    |> List.sumBy snd

// Day 5 - Part one

let entry5 = readAllLines "entry5.txt"

let parse (line: string) =
    let [part1; part2] = line |> split "->"
    let [x;y] = part1 |> split ","
    let [x';y'] = part2 |> split ","
    int x,int y,int x',int y'

let vents = entry5 |> List.map parse

let maxX = vents |> List.collect (fun (x,_,x',_) -> [x;x']) |> List.max |> ((+)1)
let maxY = vents |> List.collect (fun (_,y,_,y') -> [y;y']) |> List.max |> ((+)1)      

let diagram = Array2D.zeroCreate maxY maxX

let computePoints (diagram: int[,]) =
    diagram
        |> Seq.cast<int>
        |> Seq.filter (fun x -> x > 1)
        |> Seq.length

vents
    |> List.fold (fun diagram (x,y,x',y') ->
            if x <> x' && y <> y' then diagram
            else
                let startX, startY = min x x', min y y'
                let stopX, stopY = max x x',  max y y'
                Array2D.init maxY maxX (fun column line ->
                    if column >= startY && column <= stopY && line >= startX && line <= stopX then
                        diagram[column, line] + 1
                    else diagram[column, line])) diagram
    |> computePoints

// Day 5 - Part two

let diagram' = Array2D.zeroCreate maxY maxX

vents
    |> List.fold (fun diagram (x,y,x',y') ->
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

let entry4 = readAllLines "entry4.txt"

let numbers = entry4.[0] |> split "," |> List.map int

let boards = entry4[2..]
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

let rec play' (numbers: int list) (boards: Board list) (lastWinner: int * Board) =
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

let entry3 = readAllLines "entry3.txt"

let isBitZero = (=)'0'
let isBitOne = (=)'1'

let toBase10 bits = Convert.ToInt32(bits, 2)

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

let entry2 = readAllLines "entry2.txt" |> List.map (split " ")

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

let entry1 = readAllLines "entry1.txt" |> List.map int

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