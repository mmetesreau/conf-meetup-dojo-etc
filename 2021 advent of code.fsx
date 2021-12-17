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

// Day 18 - Part one

let entry18 = readAllLines "entry18.txt"  

// Day 18 - Part two

// Day 17 - Part one

let entry17 = readAllLines "entry17.txt"  

let [(startX, stopX);(startY, stopY)] = 
    entry17.[0]
        |> replace "target area: " ""
        |> split ","
        |> List.map (fun x -> 
              let [_;range] = x |> split "="
              let [start;stop] = range |> split ".."
              (int start, int stop))
  
type Velocity = int * int
type Position = int * int
type Range = int * int
type Area = Range * Range

let move ((x, y): Position) ((vx, vy): Velocity) =
    match vx with
    | 0 ->  (x + vx,  y + vy), (vx, vy - 1)
    | vx when vx > 0 ->  (x + vx,  y + vy), (vx - 1, vy - 1)
    | vx when vx < 0 ->  (x + vx,  y + vy), (vx + 1, vy - 1)

let isInArea (((startX, stopX), (startY, stopY)): Area) ((x, y): Position) =
    x >= startX && x <= stopX && y >= startY && y <= stopY

let wontMakeIt (((startX, stopX), (startY, stopY)): Area) ((x, y): Position) ((vx, vy): Velocity) =
    (x + vx > stopX) || (y + vy < startY) 

let launch (area: Area) (velocity: Velocity) = 
    let rec launch' (position: Position) (velocity: Velocity) (history: Position list) =
        let (x',y'), (vx',vy') = move position velocity
        let history' = (x',y')::history
        if isInArea area (x', y') then Some history'
        else
            if wontMakeIt area (x', y') (vx',vy') then None
            else launch' (x',y') (vx',vy') history'

    match launch' (0,0) velocity [(0,0)] with
    | Some (history) -> Some (velocity, history)
    | _ -> None

let vxMax = max (abs startX) (abs stopX)
let vyMax = max (abs startY) (abs stopY)

let velocities = 
    [ for vx in [-vxMax..vxMax] do
            for vy in [-vyMax..vyMax] do
                match launch ((startX, stopX), (startY, stopY)) (vx, vy) with
                | Some(velocity) -> yield velocity
                | _ -> () ]

velocities
    |> List.maxBy (fun ((_, vy), _) -> vy)
    |> (fun (_, history) -> history |> List.maxBy (fun (_, y) -> y))
    |> snd

// Day 17 - Part two

velocities |> List.length

// Day 16 - Part one

let entry16 = readAllLines "entry16.txt"  

type ParsingResult = {
    Version: int
    Value: int64
}

let toLong (bits: string) = Convert.ToInt64(bits, 2)
let toInt (bits: string) = Convert.ToInt32(bits, 2)

let toBits (hexa: string) = 
    let mapping = 
        [
            "0","0000"
            "1","0001"
            "2","0010"
            "3","0011"
            "4","0100"
            "5","0101"
            "6","0110"
            "7","0111"
            "8","1000"
            "9","1001"
            "A","1010"
            "B","1011"
            "C","1100"
            "D","1101"
            "E","1110"
            "F","1111"
        ] |> Map.ofList
    hexa |> Seq.fold (fun s x -> $"{s}{mapping.[(string x)]}") ""

let calculate (typeId: int) (subPackets: ParsingResult list) : int64 =
    let values = subPackets |> List.map (fun x -> x.Value)

    match typeId with
    | 0 ->  values |> List.fold (fun x1 x2 -> x1 + x2) 0L
    | 1 -> values |> List.fold (fun x1 x2 -> x1 * x2) 1L
    | 2 -> values |> List.min
    | 3 -> values |> List.max
    | 5 -> if values.[1] > values.[0] then 1L else 0L
    | 6 -> if values.[1] < values.[0] then 1L else 0L
    | 7 -> if values.[0] = values.[1] then 1L else 0L

let parseTypeId (packet: string) = packet.[3..5] |> toInt

let parseVersion (packet: string) = packet.[0..2] |> toInt

let parseLengthTypeId (packet: string) = packet.[6] |> (string >> toInt)

let rec parsePacket (packet: string) : ParsingResult * string = 
    let version = parseVersion packet 
    match parseTypeId packet with
    | 4 -> 
        let rec parse (bits: string) (groups: string) = 
            if bits.Length < 5 then groups, bits
            else
                let group = bits.[0..4]
                if group.[0] = '0' then $"{groups}{group.[1..]}", bits.[5..]
                else
                    parse bits.[5..] $"{groups}{group.[1..]}"

        let binaryNumber, rest = parse packet.[6..] ""
        { Version = version; Value = binaryNumber |> toLong }, rest
    | operatorTypeId ->
        match parseLengthTypeId packet  with
        | 1 ->
            let number = packet.[7..17] |> toInt
            let subPackets, rest =
                [1..number]
                    |> List.fold (fun (packets, bits) _ -> 
                        let packet, rest = parsePacket bits
                        packet::packets, rest) ([], packet.[18..])

            { Version = version + (subPackets |> List.sumBy (fun x -> x.Version));  Value = calculate operatorTypeId subPackets }, rest
        | 0 ->
            let length = packet.[7..21] |> toInt
            let subPackets, _ =
                Seq.initInfinite id
                    |> Seq.scan (fun (packets, bits) _ -> 
                                    match bits with
                                    | Some (x) when String.length x > 0 -> 
                                            let packet, rest = parsePacket x
                                            packet::packets, Some rest
                                    | _ -> packets, None) ([], Some packet.[22..length+21])
                    |> Seq.takeWhile(fun (_, rest) -> Option.isSome rest)
                    |> Seq.last

            { Version = version + (subPackets |> List.sumBy (fun x -> x.Version)); Value = calculate operatorTypeId subPackets }, packet.[length+22..]

let parsingResult, _ = 
    entry16.[0] 
        |> toBits
        |> parsePacket

parsingResult.Version

// Day 16 - Part two

parsingResult.Value

// Day 15 - Part one

let entry15 = readAllLines "entry15.txt"  

type Point = int * int

// https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
let AStart (start: Point) (goal: Point) (costs: Map<Point, int>) (getNeighbors: Point -> Point list) = 
    let inline getOrMax x map = 
        match map |> Map.tryFind x with
        | Some cost -> cost
        | _ -> Int32.MaxValue

    let mutable cameFrom = Map.empty
    let mutable gScore = Map.add start 0 Map.empty
    let mutable fScore = Map.add start (getOrMax start costs) Map.empty
    let mutable path = []
    let mutable current: Point = start

    let openSet = new PriorityQueue<Point, int>()
    openSet.Enqueue(start, getOrMax start fScore)

    while openSet.Count <> 0 do
        current <- openSet.Dequeue()
        if current = goal then 
            path <- [current]
            while cameFrom |> Map.containsKey current do
                 current <- cameFrom.[current]
                 path <- current::path
            openSet.Clear()
        else
            let neighbors = getNeighbors current
            for neighbor in neighbors do
                let score = getOrMax current gScore + getOrMax current costs
                if score < getOrMax neighbor gScore then
                    cameFrom <- Map.add neighbor current cameFrom
                    gScore <- Map.add neighbor score gScore
                    fScore <- Map.add neighbor (score + costs.[neighbor]) fScore
                    openSet.Enqueue(neighbor, getOrMax neighbor fScore)
    path

let getNeighbors (height: int) (width: int) ((x,y) : Point) = 
    [
        if x < width then yield x+1 ,y 
        if y < height then yield x, y+1 
        if x > 0 then yield x-1, y 
        if y > 0 then yield x, y-1 
    ]

let getRisks (map: int list list) = 
    [
        for y in [0..map.Length-1] do
            for x in [0..map.[0].Length-1] do
                yield (x,y), map.[y].[x]
    ] |> Map.ofList

let map = entry15 |> List.map (Seq.map (string >> int) >> List.ofSeq)
let risks = getRisks map
let topLeft = (0,0)
let bottomRight = (map.[0].Length-1, map.Length-1)

AStart topLeft bottomRight risks (getNeighbors (map.Length-1) (map.[0].Length-1))
    |> List.sumBy (fun x -> risks.[x]) 
    |> fun x -> x - risks.[topLeft]

// Day 15 - Part two

let bigMap = 
    Array.init (5*map.Length) 
        (fun y -> Array.init (5*map.[0].Length) (fun x -> 
            let risk = map.[y%map.Length].[x%map.[0].Length]
            let times =  y / map.Length + x / map.[0].Length
            [1..times] 
                |> List.fold (fun risk _ -> 
                                    match risk + 1 with
                                    | 10 -> 1
                                    | n -> n) risk))
        |> Array.map (List.ofArray) 
        |> List.ofArray

let bigRisks = getRisks bigMap
let bottomRight' = (bigMap.[0].Length-1, bigMap.Length-1)

AStart topLeft bottomRight' bigRisks (getNeighbors (bigMap.Length-1) (bigMap.[0].Length-1))
    |> List.sumBy (fun x -> bigRisks.[x]) 
    |> fun x -> x - bigRisks.[topLeft]

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

let countElements (template: Dictionary<string, float>) =
    let elements = Dictionary<string, float>()

    for pair in template.Keys do
          let element1 = $"{pair.[0]}"
          let element2 = $"{pair.[1]}"
          if elements.ContainsKey(element1) |> not then elements.Item(element1) <- 0.
          if elements.ContainsKey(element2) |> not then elements.Item(element2) <- 0.
          elements.Item(element1) <- elements.Item(element1) + template.Item(pair)
          elements.Item(element2) <- elements.Item(element2) + template.Item(pair)

    elements

let mostCommon, leastCommon = 
    [1..10]
        |> List.fold (fun template _ -> insert template) template
        |> countElements
        |> Seq.sortBy (fun x -> x.Value)
        |> (fun x -> x |> Seq.last, x |> Seq.head)

ceil (mostCommon.Value / 2.) - ceil (leastCommon.Value / 2.) |> string

// Day 14 - Part two

let mostCommon', leastCommon' = 
    [1..40]
        |> List.fold (fun template _ -> insert template) template
        |> countElements
        |> Seq.sortBy (fun x -> x.Value)
        |> (fun x -> x |> Seq.last, x |> Seq.head)

ceil (mostCommon'.Value / 2.) - ceil (leastCommon'.Value / 2.) |> string

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