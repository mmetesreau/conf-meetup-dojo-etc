// https://adventofcode.com/2021
open System
open System.IO
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Result =
  let isOk = function
    | Ok _ -> true
    | Error _ -> false

let memoization f =
    let cache = Dictionary<_,_>()
    fun c ->
        let exist, value = cache.TryGetValue c
        match exist with
        | true -> value
        | _ -> 
            let value = f c
            cache.Add (c, value)
            value

let contains (pattern: string) (str: string) = 
    str.Contains(pattern)

let split (separator: string) (str: string) = 
    str.Split(separator) |> List.ofArray

let replace (oldValue: string) (newValue: string) (str: string) = 
    str.Replace(oldValue, newValue)
    
let startWith (prefix: string) (str: string) = 
    str.StartsWith(prefix)

let empty (str: string) = 
    String.IsNullOrEmpty(str)

let ceil (x: float) = 
    Math.Ceiling(x)

let readAllLines path =
    File.ReadAllLines(path) |> List.ofArray

// Day 14 - Part one

let entry14 = readAllLines "entry14.txt"  

let rules = 
    entry14.[2..]
        |> List.map (fun x -> (split " -> " x).[0], (split " -> " x).[1])
        |> Map.ofList

let template = 
    entry14.[0]
        |> Seq.windowed 2
        |> Seq.map (fun x -> $"{x.[0]}{x.[1]}")
        |> Seq.countBy id
        |> Seq.map (fun (k, v) -> KeyValuePair(k, float v))
        |> (fun x -> Dictionary<string, float>(x))

let insert (template: Dictionary<string, float>) =
    let template' = Dictionary<string, float>()

    for pair in template.Keys do
        let pair1 = $"{pair.[0]}{rules.[pair]}"
        let pair2 = $"{rules.[pair]}{pair.[1]}"

        if template'.ContainsKey(pair1) |> not then template'.Item(pair1) <- 0.
        if template'.ContainsKey(pair2) |> not then template'.Item(pair2) <- 0.

        template'.Item(pair1) <- template'.Item(pair1) + template.Item(pair)
        template'.Item(pair2) <- template'.Item(pair2) + template.Item(pair)

    template'

let count (template: Dictionary<string, float>) =
    let count = Dictionary<string, float>()

    for pair in template.Keys do
          let pair1 = $"{pair.[0]}"
          let pair2 = $"{pair.[1]}"
          if count.ContainsKey(pair1) |> not then count.Item(pair1) <- 0.
          if count.ContainsKey(pair2) |> not then count.Item(pair2) <- 0.
          count.Item(pair1) <- count.Item(pair1) + template.Item(pair)
          count.Item(pair2) <- count.Item(pair2) + template.Item(pair)

    count

let mostCommon, leastCommon = 
    [1..10]
        |> List.fold (fun template _ -> insert template) template
        |> count
        |> Seq.sortBy (fun x -> x.Value)
        |> (fun x -> (x |> Seq.last).Value / 2. |> ceil, (x |> Seq.head).Value / 2. |> ceil)

mostCommon - leastCommon |> string

// Day 14 - Part two

let mostCommon', leastCommon' = 
    [1..40]
        |> List.fold (fun template _ -> insert template) template
        |> count
        |> Seq.sortBy (fun x -> x.Value)
        |> (fun x -> (x |> Seq.last).Value / 2. |> ceil, (x |> Seq.head).Value / 2. |> ceil)

mostCommon' - leastCommon' |> string

// Day 13 - Part one

let entry13 = readAllLines "entry13.txt"  
 
let instructionSection, dotSection = 
    entry13
        |> List.filter (empty >> not)
        |> List.partition (startWith "fold")

let dots = 
    dotSection 
        |> List.map (fun line -> 
            let [x;y] = line |> split ","
            int x, int y)
        |> Set.ofList

let instructions = 
    instructionSection
        |> List.map (fun line -> 
            let [direction;value] = line |> replace "fold along " "" |> split "="
            direction, int value)

let foldPaper (dots: (int * int) Set)  ((direction, value): (string * int)) =
    if direction = "y" then
        let top, bottom = 
            dots 
                |> Set.filter (fun (x,y) -> value <> y)
                |> Set.partition (fun (x,y) -> value > y)
        top 
            |> Set.union (bottom |> Set.map (fun (x,y) -> x, abs (y - value * 2)))
    else
        let left, right = 
            dots 
                |> Set.filter (fun (x,y) -> value <> x)
                |> Set.partition (fun (x,y) -> value > x)
        left
            |> Set.union (right |> Set.map (fun (x,y) -> abs (x - value * 2), y))

instructions.[0]  
    |> foldPaper dots
    |> Set.count

// Day 13 - Part two

let print (dots: (int * int) Set) =
    let maxX, maxY = dots |> Set.map (fst) |> Set.maxElement, dots |> Set.map (snd) |> Set.maxElement
    printfn ""
    for y in [0..maxY] do 
        printfn ""
        for x in [0..maxX] do
            if dots |> Set.exists (fun dot -> dot = (x,y)) then printf "#"
            else printf "."
    printfn ""

instructions  
    |> List.fold foldPaper dots
    |> print

// Day 12 - Part one

let entry12 = readAllLines "entry12.txt"  

let starts = 
    entry12 
        |> List.filter (contains "start")
        |> List.collect (split "-")
        |> List.filter ((<>)"start")

let ends = 
    entry12 
        |> List.filter (contains "end")
        |> List.collect (split "-")
        |> List.filter ((<>)"end")

let connections = 
    entry12 
        |> List.filter (fun token -> token |> (contains "start" >> not) && token |> (contains "end" >> not))
        |> List.collect (fun token -> 
            let [cave;cave'] = token |> split "-"
            [cave, cave';cave', cave])
        |> List.groupBy fst
        |> Map.ofList

let isSmallCave (cave: string) = cave <> cave.ToUpper()

let rec visit (connections: Map<string, (string * string) list>) (path: string list) (cave: string) = 
    [
        if (isSmallCave >> not) cave || path |> (List.contains cave >> not) then

            if ends |> List.contains cave then yield cave::path

            for _, cave' in connections.[cave] do
                if isSmallCave cave' then yield! visit connections (cave::path) cave'
                else yield! visit connections (cave::path) cave'
    ]

starts
    |> List.collect (visit connections [])
    |> List.distinct
    |> List.length

// Day 12 - Part two

let rec visit' (connections: Map<string, (string * string) list>) (path: string list) (cave: string) = 
    [
         if (isSmallCave >> not) cave || path |> (List.contains cave >> not)
            || path |> List.filter isSmallCave |> List.countBy id |> (List.exists (snd >> ((<)1)) >> not) then

            if ends |> List.contains cave then yield cave::path

            for _, cave' in connections.[cave] do
                if isSmallCave cave' then yield! visit' connections (cave::path) cave'
                else yield! visit' connections (cave::path) cave'
    ]

starts
    |> List.collect (visit' connections [])
    |> List.distinct
    |> List.length

// Day 11 - Part one

let entry11 = readAllLines "entry11.txt" |> List.map (Seq.map (string >> int) >> List.ofSeq)

let adjacents' (height: int) (width: int) ((x,y) : int * int) = 
    [
        if x > 0 then yield x-1, y 
        if x < width then yield x+1 ,y 
        if y > 0 then yield x, y-1 
        if y < height then yield x, y+1 
        
        if x < width && y < height then yield x+1, y+1 
        if x < width && y > 0 then yield x+1, y-1 
        if x > 0  && y > 0 then yield x-1, y-1 
        if x > 0  && y < height then yield  x-1, y+1 
    ]

let adjacents'' = adjacents' (entry11.Length-1) (entry11.[0].Length-1)

let tick (octopuses: Map<(int * int), int>) =
    let rec tick' (octopuses: Map<(int * int), int>) (positions: (int*int) list) =
        let octopuses' = octopuses |> Map.map (fun position energy -> energy + (positions |> List.filter ((=) position) |> List.length))
        let flashing = positions |> List.filter (fun x -> octopuses'.[x] > 9) |> List.distinct
        let positions' = 
           flashing
                |> List.collect adjacents''
                |> List.filter (fun x -> octopuses'.[x] <= 9)

        if positions'.Length = 0 then octopuses' |> Map.map (fun _ energy -> if energy > 9 then 0 else energy)
        else tick' octopuses' positions'
    
    tick' octopuses (List.ofSeq octopuses.Keys)
    
let octopuses = 
    [
        for y in [0..entry11.Length-1] do
             for x in [0..entry11.[0].Length-1] -> (x,y), entry11.[y].[x]
    ] |> Map.ofList

let countFlash (octopuses: Map<int * int, int>) = 
    octopuses
        |> Map.filter (fun _ energy -> energy = 0)
        |> Map.count

[1..100]
    |> List.scan (fun octopuses _ -> tick octopuses) octopuses
    |> List.map countFlash
    |> List.sum

// Day 11 - Part two

Seq.initInfinite id
    |> Seq.scan (fun (step, octopuses) _ -> step+1, (tick octopuses)) (0, octopuses)
    |> Seq.map (fun  (step, octopuses) -> step, octopuses |> countFlash)
    |> Seq.takeWhile (fun (_, flashes) -> flashes <> 100)
    |> List.ofSeq
    |> List.last
    |> (fst >> ((+)1))

// Day 10 - Part one

let entry10 = readAllLines "entry10.txt" |> List.map (List.ofSeq)

let pairs = [ '{','}';'[',']';'<','>'; '(',')'] |> Map.ofList

let (|OpeningCharacter|ClosingCharacter|) character =
    if pairs |> Map.containsKey character then OpeningCharacter
    else ClosingCharacter

let parseCharacter (openedCharacters: char list) (character: char) =
    match character with
    | OpeningCharacter -> character::openedCharacters |> Ok
    | ClosingCharacter -> 
        if character = pairs.[openedCharacters.[0]] then openedCharacters.Tail |> Ok
        else character |> Error

let parseChunk (chunck: char list) =
    chunck 
        |> List.fold (fun parsingResult character ->
            match parsingResult with
            | Ok openedCharacters -> parseCharacter openedCharacters character 
            | Error e -> Error e) (Ok [])

let errorScores = [')', 3L; ']', 57L; '}', 1197L; '>', 25137L] |> Map.ofList

let incompleteChuncks, corruptedChunks = 
    entry10 
        |> List.map parseChunk
        |> List.partition Result.isOk

corruptedChunks |> List.sumBy (fun (Error error) -> errorScores.[error])

// Day 10 - Part two

let charactereScores = [')',1L;']',2L;'}',3L;'>',4L] |> Map.ofList

incompleteChuncks
    |> List.map (fun (Ok missingCharacters) -> missingCharacters |> List.map (fun x -> pairs.[x]))
    |> List.map (List.fold (fun score character -> score * 5L + charactereScores.[character]) 0L)
    |> List.sort
    |> fun scores -> scores.[scores.Length/2]

// Day 9 - Part one

let entry = readAllLines "entry9.txt"  
                |> List.map (Seq.map (string >> int) >> List.ofSeq)

let adjacents (heightmap: int list list) ((x,y) : int * int) = 
    let height, width = entry.Length-1, entry.[0].Length-1
    [
        if x > 0 then yield heightmap.[y].[x-1], x-1, y 
        if x < width then yield heightmap.[y].[x+1], x+1 ,y 
        if y > 0 then yield heightmap.[y-1].[x], x, y-1 
        if y < height then yield heightmap.[y+1].[x], x, y+1 
    ]

let lowPoints = 
    [ 
        for y in [0..entry.Length-1] do
            for x in [0..entry.[0].Length-1] do
                if adjacents entry (x, y) |> List.forall (fun (v, _, _)-> entry.[y].[x] < v) then
                    yield x, y
    ]

lowPoints |> List.sumBy (fun (x, y) -> entry.[y].[x] + 1)

// Day 9 - Part two

let rec findBasin (heightmap: int list list) (basin: Set<int * int>) ((x,y) : int * int) =
    if basin |> Set.contains (x, y) then basin
    else
        let basin' = basin |> Set.add (x, y) 
        adjacents heightmap (x, y)
            |> List.filter (fun (v, _, _) -> v < 9)
            |> List.fold (fun basin (_, x', y') -> findBasin heightmap basin (x', y') ) basin'

lowPoints
    |> List.map (findBasin entry Set.empty >> Set.count)
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)

// Day 8 - Part one

let entry8 = 
    readAllLines "entry8.txt" 
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
    |> List.fold (fun minFuel targetPosition -> (crabPositions |> List.sumBy (computeFuelConsumption'M targetPosition), minFuel) ||> min) Int32.MaxValue

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