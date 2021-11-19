// https://adventofcode.com/2020
open System
open System.IO
open System.Text.RegularExpressions

let readAllText path =
    File.ReadAllText(path)

let readAllLines path =
    File.ReadAllLines(path)

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

// Day 16 - Part one

let day16 = [
    "departure location: 44-825 or 849-962"
    "departure station: 26-296 or 316-965"
    "departure platform: 46-889 or 896-949"
    "departure track: 48-351 or 369-960"
    "departure date: 25-869 or 884-966"
    "departure time: 31-217 or 232-956"
    "arrival location: 32-559 or 574-967"
    "arrival station: 50-383 or 394-952"
    "arrival platform: 29-128 or 150-962"
    "arrival track: 30-630 or 647-957"
    "class: 45-262 or 277-966"
    "duration: 35-602 or 619-965"
    "price: 41-913 or 926-966"
    "route: 38-191 or 212-950"
    "row: 25-509 or 523-965"
    "seat: 39-783 or 802-973"
    "train: 36-64 or 80-969"
    "type: 42-750 or 767-974"
    "wagon: 29-803 or 821-974"
    "zone: 47-659 or 672-968"
]

let tickets = 
    day16 
        |> Seq.map (fun rule -> 
                        let tokens = rule |> split ":"
                        tokens.[0], tokens.[1] )
        |> Seq.map (fun (field, ranges) ->
                        let tokens = ranges |> split "or"
                        field, tokens 
                                    |> Seq.map (fun range -> 
                                                        let tokens = range |> trim |> split("-")
                                                        tokens.[0] |> int, tokens.[1] |> int)
                                    |> List.ofSeq)
        |> List.ofSeq

readAllLines (sprintf "%s/day16.txt" __SOURCE_DIRECTORY__)
|> Seq.collect (fun ticket -> ticket |> split "," |> Seq.map int |> List.ofSeq)
|> Seq.filter(fun fieldValue -> tickets |> List.exists(fun (_, ranges) -> ranges |> List.exists (fun (min, max) -> fieldValue >= min && fieldValue <= max )) = false)
|> Seq.sum        

// Day 16 - Part two

// Day 15 - Part one

// Day 15 - Part two

// Day 14 - Part one

let day14 = 
    readAllLines (sprintf "%s/day14.txt" __SOURCE_DIRECTORY__)

let initialize instructions = 
    let rec initialize' mask memory instructions =
        match instructions with
        | [] -> memory
        | instruction:: instructions ->
            if instruction |> startWith "mask" then
                let mask' = instruction |> substr2 7
                initialize' mask' memory instructions
            else
                let value = instruction |> split "=" |> List.item 1 |> trim
                let position = instruction |> split "["  |> List.item 1 |> split "]" |> List.item 0
                let value' =
                    Convert
                        .ToString(int value, 2)
                        .PadLeft(mask.Length, '0')
                            |> Seq.mapi (fun position bit ->
                                                match (mask.[position],bit) with
                                                | ('X', b) -> b
                                                | (m, _) -> m)
                            |> join
                            |> fun bits -> Convert.ToInt64(bits, 2)

                let memory' = memory |> Map.add position value'

                initialize' mask memory' instructions

    initialize' "" Map.empty instructions            
        
day14
    |> List.ofSeq
    |> initialize
    |> Map.fold (fun state _ value -> state + value) 0L

// Day 14 - Part two

let rec computeAddresses (addresses: char list list) (bits: char list)  = 
    match bits with
    | [] -> addresses
    | bit::bits -> 
        let bitValues = if bit = 'X' then ['0'; '1'] else [bit] 
        bitValues 
            |> List.collect (fun bitValue -> computeAddresses (addresses |> List.map (fun address -> bitValue::address)) bits)

let initialize2 instructions = 
    let rec initialize' mask memory instructions =
        match instructions with
        | [] -> memory
        | instruction:: instructions ->
            if instruction |> startWith "mask" then
                let mask' = instruction |> substr2 7
                initialize' mask' memory instructions
            else
                let value = instruction |> split "=" |> List.item 1 |> trim
                let position = instruction |> split "["  |> List.item 1 |> split "]" |> List.item 0
                let memory' =
                    Convert
                        .ToString(int position, 2)
                        .PadLeft(mask.Length, '0')
                            |> Seq.mapi (fun position bit ->
                                                match (mask.[position],bit) with
                                                | ('X', _) -> 'X'
                                                | ('1', _) -> '1'
                                                | (_, '1') -> '1'
                                                | _ -> '0')
                            |> join
                            |> List.ofSeq
                            |> computeAddresses [[]]
                            |> List.fold (fun memory address -> memory |> Map.add address (int64 value)) memory

                initialize' mask memory' instructions

    initialize' "" Map.empty instructions 

day14
    |> List.ofSeq
    |> initialize2
    |> Map.fold (fun state _ value -> state + value) 0L    
  
// Day 13 - Part one

let day13 = 
    readAllLines (sprintf "%s/day13.txt" __SOURCE_DIRECTORY__)

let ts = day13.[0] |> int

day13.[1]
    |> split ","
    |> Seq.filter ((<>)"x")
    |> Seq.map int
    |> Seq.sortBy (fun busId -> ts/busId)
    |> Seq.head
    |> fun busId -> busId * (( busId + busId * (ts/busId)) - ts)

// Day 13 - Part two

// TO CLEAN

let busIds =
    day13.[1]
    "17,x,13,19"
        |> split ","
        |> Seq.indexed
        |> Seq.filter (fun (_, busId) -> busId <> "x")
        |> Seq.map (fun (index, busId) -> float index, float busId)
        |> List.ofSeq

let mutable (o1,  v1) = busIds |> List.head
let mutable n1 = v1

for i in [1..busIds.Length-1] do
    let (o2, v2) = busIds.[i]
    let n2 = 0.;
    while (n1 + o2) % v2 <> 0. do
      n1 <- n1 + v1;
    v1 <- v1 * v2;

int64 n1

// Day 12 - Part one

let day12 = 
    readAllLines (sprintf "%s/day12.txt" __SOURCE_DIRECTORY__)

let rec navigate (n, s, e, w) direction instructions = 
        match instructions with
        | [] -> (n, s, e, w)
        | instruction::instructions ->
            let action = instruction |> substr 0 1
            let value = instruction |> substr 1 (instruction.Length-1) |> int
            match action with
            | "F" -> 
                match direction with
                | "N" -> navigate (n + value, s, e, w) direction instructions
                | "S" -> navigate (n, s + value, e, w) direction instructions
                | "E" -> navigate (n, s, e + value, w) direction instructions
                | "W" -> navigate (n, s, e, w + value) direction instructions
                | _ -> failwith "oups"
            | "N" -> navigate (n + value, s, e, w) direction instructions
            | "S" -> navigate (n, s + value, e, w) direction instructions
            | "E" -> navigate (n, s, e + value, w) direction instructions
            | "W" -> navigate (n, s, e, w + value) direction instructions
            | "R" -> 
                let directions = "NESW"
                let direction' = string directions.[(((value%360)/90) + directions.IndexOf(direction)) % 4]
                navigate (n, s, e, w) direction' instructions
            | "L" -> 
                let directions = "NWSE"
                let direction' = string directions.[(((value%360)/90) + directions.IndexOf(direction)) % 4]
                navigate (n, s, e, w) direction' instructions
            | _ -> failwith "oups"

day12
    |> List.ofArray
    |> navigate (0, 0, 0, 0) "E"
    |> fun (n, s, e, w) -> max n s - min n s + max w e - min w e

// Day 12 - Part two

let rec waypointNavigation (n, s, e, w) (wn, ws, we, ww) instructions = 
    match instructions with
    | [] -> (n, s, e, w)
    | instruction::instructions ->
        let action = instruction |> substr 0 1
        let value = instruction |> substr 1 (instruction.Length-1) |> int
        match action with
        | "F" -> waypointNavigation (n + (value * wn), s + (value * ws), e + (value * we), w + (value * ww)) (wn, ws, we, ww) instructions
        | "N" -> waypointNavigation (n, s, e, w) (wn + value, ws, we, ww) instructions
        | "S" -> waypointNavigation (n, s, e, w) (wn, ws + value, we, ww) instructions
        | "E" -> waypointNavigation (n, s, e, w) (wn, ws, we + value, ww) instructions
        | "W" -> waypointNavigation (n, s, e, w) (wn, ws, we, ww + value) instructions
        | "R" -> 
            let (wn', ws', we', ww') = 
                match (value%360) with 
                | 0 -> (wn, ws, we, ww)
                | 90 -> (ww, we, wn, ws)
                | 180 -> (ws, wn, ww, we)
                | 270 -> (we, ww , ws, wn)
                | 360 -> (wn, ws, we, ww)
            waypointNavigation (n, s, e, w) (wn', ws', we', ww') instructions
        | "L" -> 
            let (wn', ws', we', ww') = 
                match (value%360) with 
                | 0 -> (wn, ws, we, ww)
                | 90 -> (we, ww, ws, wn)
                | 180 -> (ws, wn, ww, we)
                | 270 -> (ww, we, wn, ws)
                | 360 -> (wn, ws, we, ww)
            waypointNavigation (n, s, e, w) (wn', ws', we', ww') instructions
        | _ -> failwith "oups"

day12
    |> List.ofArray
    |> waypointNavigation (0, 0, 0, 0) (1, 0, 10, 0)
    |> fun (n, s, e, w) -> max n s - min n s + max w e - min w e

// Day 11 - Part one

let day11 = 
    readAllLines (sprintf "%s/day11.txt" __SOURCE_DIRECTORY__)

let countNumberOfAdjacentOccupiedSeat x y (layout: string []) = 
    let leftSeatExists = x + 1 < layout.[0].Length
    let rightSeatExists =  x - 1 > -1 
    let topSeatExists = y - 1 > -1
    let bottomSeatExist = y + 1 < layout.Length
    let isOccupied seat = seat = '#'

    let countLeft = if leftSeatExists && layout.[y].[x + 1] |> isOccupied then 1 else 0
    let countRight = if rightSeatExists && layout.[y].[x - 1] |> isOccupied then 1 else 0
    let countTop = if topSeatExists && layout.[y - 1].[x] |> isOccupied then 1 else 0
    let countBottom = if bottomSeatExist && layout.[y + 1].[x] |> isOccupied then 1 else 0
    let countTopLeft = if leftSeatExists && topSeatExists && layout.[y - 1].[x + 1] |> isOccupied then 1 else 0
    let countTopRight = if rightSeatExists && topSeatExists && layout.[y - 1].[x - 1] |> isOccupied then 1 else 0
    let countBottomLeft = if leftSeatExists && bottomSeatExist && layout.[y + 1].[x + 1] |> isOccupied then 1 else 0
    let countBottomRight = if rightSeatExists && bottomSeatExist && layout.[y + 1].[x - 1] |> isOccupied then 1 else 0
    
    countLeft + countRight + countTop + countBottom + countTopLeft + countTopRight + countBottomLeft + countBottomRight

let rules x y (layout: string []) = 
    let numberOfAdjacentOccupiedSeat = countNumberOfAdjacentOccupiedSeat x y layout
    match layout.[y].[x] with
    | 'L' -> if numberOfAdjacentOccupiedSeat = 0 then "#" else "L"
    | '#' -> if numberOfAdjacentOccupiedSeat >= 4 then "L" else "#"
    | other -> string other

let rec reachEquilibrium rules (layout: string []) =
    let processLine y = 
        [0..(layout.[0].Length-1)] 
            |>  Seq.fold (fun line' x -> rules x y layout |> sprintf "%s%s" line' ) ""

    let layout' = [|0..(layout.Length-1)|] 
                        |> Array.map processLine

    let count = layout |> Array.sumBy (fun line -> line |> Seq.filter (fun y -> y = '#') |> Seq.length) 
    let count' = layout' |> Array.sumBy (fun line -> line |> Seq.filter (fun y -> y = '#') |> Seq.length) 

    if count = count' then count else reachEquilibrium rules layout'

reachEquilibrium rules day11

// Day 11 - Part two

let countNumberOfOccupiedSeatFirstSeen x y (layout: string []) = 
    let leftSeatExists = x + 1 < layout.[0].Length
    let rightSeatExists =  x - 1 > -1 
    let topSeatExists = y - 1 > -1
    let bottomSeatExist = y + 1 < layout.Length
    let firstSeatIsOccupied seats = seats |> isMatch "^(\.)*#.*$"

    let countLeft = if leftSeatExists && [x+1..layout.[0].Length-1] |> List.fold (fun state x' -> sprintf "%s%c" state layout.[y].[x']) "" |> firstSeatIsOccupied then 1 else 0
    let countRight = if rightSeatExists && [0..x-1] |> List.fold (fun state x' -> sprintf "%c%s" layout.[y].[x'] state) "" |> firstSeatIsOccupied then 1 else 0
    let countTop = if topSeatExists && [0..y-1] |> List.fold (fun state y' -> sprintf "%c%s"  layout.[y'].[x] state) "" |> firstSeatIsOccupied then 1 else 0
    let countBottom = if bottomSeatExist && [y+1..layout.Length-1] |> List.fold (fun state y' -> sprintf "%s%c" state layout.[y'].[x]) "" |> firstSeatIsOccupied then 1 else 0
    let countTopLeft = if leftSeatExists && topSeatExists && [1..(min (y) (layout.[0].Length-x-1))] |> List.fold (fun state cpt  -> sprintf "%s%c" state layout.[y-cpt].[x+cpt] ) "" |> firstSeatIsOccupied then 1 else 0
    let countTopRight = if rightSeatExists && topSeatExists && [1..(min (x) (y))] |> List.fold (fun state cpt  -> sprintf "%s%c" state layout.[y-cpt].[x-cpt]) ""  |> firstSeatIsOccupied then 1 else 0
    let countBottomLeft = if leftSeatExists && bottomSeatExist && [1..(min (layout.Length-y-1) (layout.[0].Length-x-1))] |> List.fold (fun state cpt -> sprintf "%s%c" state layout.[y+cpt].[x+cpt] ) "" |> firstSeatIsOccupied then 1 else 0
    let countBottomRight = if rightSeatExists && bottomSeatExist && [1..(min (layout.Length-y-1) (x))] |> List.fold (fun state cpt -> sprintf "%s%c" state layout.[y+cpt].[x-cpt] ) "" |> firstSeatIsOccupied then 1 else 0
    
    countLeft + countRight + countTop + countBottom + countTopLeft + countTopRight + countBottomLeft + countBottomRight

let tolerantRules x y (layout: string []) = 
    let numberOfOccupiedSeatFirstSeen = countNumberOfOccupiedSeatFirstSeen x y layout
    match layout.[y].[x] with
    | 'L' -> if numberOfOccupiedSeatFirstSeen = 0 then "#" else "L"
    | '#' -> if numberOfOccupiedSeatFirstSeen >= 5 then "L" else "#"
    | other -> string other

reachEquilibrium tolerantRules day11

// Day 10 - Part one

let day10 = 
    readAllLines (sprintf "%s/day10.txt" __SOURCE_DIRECTORY__)
    
// TO CLEAN

// Day 10 - Part two

// TO CLEAN

// Day 9 - Part one

let day9 = 
    readAllLines (sprintf "%s/day9.txt" __SOURCE_DIRECTORY__)
    |> Seq.map int64

let preambleLength = 25

let firstInvalidNumber = 
    day9
        |> Seq.skip(preambleLength)
        |> Seq.indexed
        |> Seq.filter (fun (index, sum) -> 
                                        let preamble = 
                                            day9 
                                                |> Seq.skip (index) 
                                                |> Seq.take preambleLength
                                        
                                        preamble
                                            |> Seq.tryFind (fun number -> number <> (sum - number) && preamble |> Seq.contains (sum - number))
                                            |> Option.isNone)
        |> Seq.map snd
        |> Seq.head

// Day 9 - Part two

let day9' = 
    day9 
        |> List.ofSeq
        |> List.filter ((<>)firstInvalidNumber)

let find sum startIndex  =
    let rec find' index numbers  =
        let numbers' = day9'.[index] :: numbers 
        let sum' = numbers' |> Seq.sum

        if sum' = sum then numbers' |> Some
        elif sum'> sum then None
        else find' (index + 1) numbers'

    find' startIndex [] 

[0.. (day9'.Length-1)]
    |> List.map (fun (startIndex) -> find firstInvalidNumber startIndex)
    |> List.filter Option.isSome  
    |> List.collect Option.get
    |> List.sort 

// Day 8 - Part one

let day8 = 
    readAllLines (sprintf "%s/day8.txt" __SOURCE_DIRECTORY__)

let instructions = 
    day8
        |> Seq.map (fun code -> 
                                match code |> split " " with
                                | [instruction;value] -> instruction, value
                                | _  -> failwith "oups") 
        |> List.ofSeq

let rec hasInfiniteLoop (next: int) (acc: int) (history: int Set) (instructions: (string * string) list)  = 
    if history |> Set.contains next then (true, acc)
    else
        let (cmd, value) = instructions.[next]
        let (next', acc', history') =
            match cmd with 
            | "jmp" -> (next + int value), acc,  history |> Set.add next
            | "acc" -> (next + 1), (acc + int value),  history |> Set.add next
            | "nop" -> (next + 1), acc,  history |> Set.add next
            | _ -> failwith "oups"
        if next = (instructions.Length - 1) then (false, acc')
        else hasInfiniteLoop next' acc' history' instructions
 
instructions |> hasInfiniteLoop 0 0 Set.empty 

// Day 8 - Part two

[
    for cpt in [0..(instructions.Length-1)] do
      match instructions.[cpt] with
      | "jmp", value ->
            yield instructions
            yield instructions |> List.mapi (fun index elt -> if index = cpt then ("nop", value) else elt)
      | "nop", value ->
          yield instructions
          yield instructions |> List.mapi (fun index elt -> if index = cpt then ("jmp", value) else elt)
      | _, _ ->  () 
] 
    |> Seq.map (fun instructions -> instructions |> hasInfiniteLoop 0 0 Set.empty )
    |> Seq.filter (fun (hasInfiniteLoop, acc) -> hasInfiniteLoop |> not)

// Day 7 - Part one

let day7 = 
    readAllLines (sprintf "%s/day7.txt" __SOURCE_DIRECTORY__)

let regex = Regex("^(?<container>(\w+\s)+)bags contain ((?<quantity>[0-9]+)\s(?<contained>(\w+\s)+)bag(s)?(,\s)?)+\.$", RegexOptions.Compiled)

let getBagsThatCanContainAtLeast bag rules =
    let lookup =
        rules
            |> Seq.collect (fun rule -> 
                                let tokens = regex.Match(rule)
                                tokens
                                    .Groups.["contained"]
                                    .Captures
                                        |> Seq.mapi(fun index content -> (content.Value.Trim(), tokens.Groups.["container"].Value.Trim())))
            |> Seq.groupBy fst
            |> Map.ofSeq

    let rec getBagsThatCanContainAtLeast' bag = 
        if lookup |> Map.containsKey bag then
            lookup 
                |> Map.find bag
                |> List.ofSeq
                |> List.collect (fun (_, bag') -> bag'::(getBagsThatCanContainAtLeast' bag'))
        else []
    
    getBagsThatCanContainAtLeast' bag

day7    
    |> getBagsThatCanContainAtLeast "shiny gold"
    |> Seq.distinct
    |> Seq.length
                                             
// Day 7 - Part two
    
let countRequiredBagFor bag rules =
    let lookup =
        rules
            |> Seq.collect (fun rule -> 
                                let tokens = regex.Match(rule)
                                tokens
                                    .Groups.["contained"]
                                    .Captures
                                        |> Seq.mapi(fun index content -> (tokens.Groups.["container"].Value.Trim(), (tokens.Groups.["quantity"].Captures.[index].Value |> int, content.Value.Trim()))))
            |> Seq.groupBy fst
            |> Map.ofSeq
     
    let rec countRequiredBagFor' bag =
    
        if lookup |> Map.containsKey bag then
            lookup 
                |> Map.find bag
                |> List.ofSeq
                |> List.sumBy (fun (_, (quantity, bag')) -> quantity + quantity * countRequiredBagFor' bag')
        else 0

    countRequiredBagFor' bag

day7 |> countRequiredBagFor "shiny gold"

// Day 6 - Part one

let day6 = 
    readAllText (sprintf "%s/day6.txt" __SOURCE_DIRECTORY__)

let groupsAnswers =
    day6
        |> split "\r\n\r\n"
        |> Seq.map (fun group -> group |> split "\r\n")

let countALeastOneYes groupAnswers =
    groupAnswers 
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.length

groupsAnswers 
    |> Seq.sumBy countALeastOneYes

// Day 6 - Part two

let countAllYes groupAnswers =
    groupAnswers 
        |> Seq.collect id
        |> Seq.groupBy id
        |> Seq.filter (fun (_, answers) -> (answers |> Seq.length) = (groupAnswers |> Seq.length))
        |> Seq.length

groupsAnswers
    |> Seq.sumBy countAllYes

// Day 5 - Part one

let day5 = 
    readAllLines (sprintf "%s/day5.txt" __SOURCE_DIRECTORY__)

let getSeatId (row, column) = 
    row * 8 + column

let getSeatRowColumn (boardingPass: string) =
    let rec getSeatRowColumn' minRow maxRow minColumn maxColumn characters =
        match characters with
        | characters when Seq.isEmpty characters -> (minRow, minColumn)
        | characters ->
            let (minRow', maxRow', minColumn', maxColumn') = 
                match characters |> Seq.head with
                | 'B' -> minRow + (maxRow - minRow) / 2 + 1, maxRow, minColumn, maxColumn
                | 'F' -> minRow, maxRow - (maxRow - minRow) / 2 - 1, minColumn, maxColumn
                | 'R' -> minRow, maxRow, minColumn + (maxColumn - minColumn) / 2 + 1, maxColumn
                | 'L' -> minRow, maxRow, minColumn, maxColumn - (maxColumn - minColumn) / 2 - 1
                | _ -> failwith "oups"
            
            getSeatRowColumn' minRow' maxRow' minColumn' maxColumn' (characters |> Seq.tail)

    getSeatRowColumn' 0 127 0 7 boardingPass

let seats = 
    day5 |> Seq.map getSeatRowColumn

let seatIds = 
    seats |> Seq.map getSeatId

seatIds |> Seq.max

// Day5 - Part two

let all = 
    [
        for x in [1..126] do
            for y in [0..7] do
                (x,y)
        ]  |> Set.ofList

seats 
    |> Set.ofSeq
    |> Set.difference (all)
    |> Seq.map getSeatId
    |> Seq.filter (fun seatId -> seatIds |> Seq.contains (seatId + 1) && seatIds |> Seq.contains (seatId - 1))
    |> Seq.head

// Day 4 - Part one

let day4 = 
    readAllText (sprintf "%s/day4.txt" __SOURCE_DIRECTORY__)

let requiredFields = [
    "byr"
    "iyr"
    "eyr" 
    "hgt"
    "hcl"
    "ecl"
    "pid"
]

let passports = 
    day4
        |> split "\r\n\r\n"
        |> Seq.map (fun passport -> passport 
                                        |> split "\r\n"
                                        |> Seq.collect (fun fields -> fields |> split " ")
                                        |> Seq.map (fun field -> (field |> split ":").[0], (field |> split ":").[1])
                                        |> Map.ofSeq)

let passportsWithRequiredFields =
    passports
        |> Seq.filter (fun passport -> requiredFields |> List.forall (fun requiredField -> passport |> Map.containsKey requiredField ))

passportsWithRequiredFields |> Seq.length

// Day 4 - Part two

let isValidPassport fields =
    let isBetween min max value = value >= min && value <= max

    fields
        |> Map.forall (fun field value -> 
                            match field with
                            | "byr" -> value |> isMatch "^[0-9][0-9][0-9][0-9]$" && value |> int |> isBetween 1920 2002
                            | "iyr" -> value |> isMatch "^[0-9][0-9][0-9][0-9]$" && value |> int |> isBetween 2010 2020
                            | "eyr" -> value |> isMatch "^[0-9][0-9][0-9][0-9]$" && value |> int |> isBetween 2020 2030
                            | "hcl" -> value |> isMatch "^#([0-9]|[a-z])([0-9]|[a-z])([0-9]|[a-z])([0-9]|[a-z])([0-9]|[a-z])([0-9]|[a-z])$"
                            | "ecl" -> ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> List.contains value
                            | "pid" -> value |> isMatch "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"
                            | "hgt" when  value |> contains "cm" -> value |> replace "cm" "" |> int |> isBetween 150 193
                            | "hgt" when  value |> contains "in" -> value |> replace "in" "" |> int |> isBetween 59 76
                            | _ -> true)

passportsWithRequiredFields
    |> Seq.filter isValidPassport
    |> Seq.length

// Day 3 - Part one

let day3 = 
    readAllLines (sprintf "%s/day3.txt" __SOURCE_DIRECTORY__)

let countTrees (right: int) (down: int) (tobogan: string []) =
    let rec countTrees' (x, y) trees =
        if y < tobogan.Length then
            let trees' = 
                if tobogan.[y].[x] = '#' then trees + 1UL
                else trees
            countTrees' ((x + right) % tobogan.[0].Length, y + down) trees'
        else trees
    
    countTrees' (0,0) 0UL

countTrees 3 1 day3

// Day 3 - Part two

(countTrees 1 1 day3) * (countTrees 3 1 day3) * (countTrees 5 1 day3) * (countTrees 7 1 day3) * (countTrees 1 2 day3) 

// Day 2 - Part one

let day2 = 
    readAllLines (sprintf "%s/day2.txt" __SOURCE_DIRECTORY__)

let passwordPolicies = 
    day2
        |> Seq.map (fun line -> 
                            let tokens = split " " line
                            let minMax = split "-" tokens.[0]  
                            (int minMax.[0], int minMax.[1], tokens.[1] |> replace ":" "", tokens.[2]))

passwordPolicies
    |> Seq.filter(fun policy -> 
                        let (min, max, letter, password) = policy
                        let countLetter = password |> Seq.filter (fun x -> (string x) = letter) |> Seq.length
                        min <= countLetter && max >= countLetter)
    |> Seq.length

// Day 2 - Part two

passwordPolicies
    |> Seq.filter(fun policy -> 
                        let (position1, position2, letter, password) = policy
                        match string password.[position1 - 1], string password.[position2 - 1] with
                        | (x, y) when x = letter && y <> letter -> true
                        | (x, y) when x <> letter && y = letter -> true
                        | _ -> false )
    |> Seq.length

// Day 1 - Part one

let day1 = 
    readAllLines (sprintf "%s/day1.txt" __SOURCE_DIRECTORY__) |> Seq.map int


let tryFindNumbersThatSumTo sum candidates =
    let lookup = 
        Set.ofSeq day1

    candidates
        |> Seq.filter (fun x -> x < sum)
        |> Seq.tryPick (fun candidate -> 
                                match lookup |> Set.contains(sum - candidate) with 
                                | true -> Some [candidate; sum - candidate] 
                                | _ -> None)
 
match tryFindNumbersThatSumTo 2020 day1 with
| Some numbers -> Seq.fold (*) 1 numbers
| None -> failwith "oups"

// Day 1 - Part two

day1
    |> Seq.pick (fun candidate -> 
                            match tryFindNumbersThatSumTo (2020 - candidate) day1 with
                            | None -> None
                            | Some numbers -> candidate::numbers |> Some)
    |> Seq.fold (*) 1
