open System

// --- Day 1: No Time for a Taxicab ---

// type Point = int
// type Distance = int
// type Direction = L | R
// type Instruction = Direction * Distance
// type Orientation = N | S | E | W
// type Position = {
//         orientation: Orientation
//         x: Point
//         y: Point
//     }
//     with static member InitialPosition() = { orientation = Orientation.N; x = 0; y = 0 }

// let parseInstructions (instructions: string) : Instruction list =
//     let parseDistance (distance: string) : Distance = int distance

//     let parseDirection (direction: string) : Direction =
//         match direction with
//         | "L" -> L
//         | "R" -> R
//         | _ -> failwith "..."

//     let sanityzeInstructions (instructions: string) : string = instructions.Replace(" ", "")

//     let splitInstructions (separator: char) (instructions: string) : string list = instructions.Split(separator) |> List.ofSeq

//     let parseInstruction (instruction: string) : Instruction =
//         instruction |> Seq.head |> string |> parseDirection, instruction |> Seq.tail |> Seq.map string |> String.concat "" |> parseDistance

//     instructions
//         |> sanityzeInstructions
//         |> splitInstructions ','
//         |> List.map parseInstruction

// let applyInstruction (position: Position) (direction, distance) : Position =
//     match position.orientation, direction with
//     | S, L | N, R -> { position with orientation = E; x = position.x + distance }
//     | W, L | E, R -> { position with orientation = S; y = position.y - distance }
//     | S, R | N, L -> { position with orientation = W; x = position.x - distance }
//     | W, R | E, L -> { position with orientation = N; y = position.y + distance }

// let computeDistance (position: Position) : Distance =
//     let initialPosition = Position.InitialPosition()
//     abs (initialPosition.x - position.x) + abs (initialPosition.y - position.y)

// let run instructions = instructions
//                         |> parseInstructions
//                         |> List.fold applyInstruction (Position.InitialPosition())
//                         |> computeDistance

// // --- Day 2: Bathroom Security ---

// open System

// type Button = { x:int; y:int }

// type Instruction =
//     | Up
//     | Down
//     | Left
//     | Right

// let keypad = [
//                 {x=0;y=0}, "1"
//                 {x=1;y=0}, "2"
//                 {x=2;y=0}, "3"
//                 {x=0;y=1}, "4"
//                 {x=1;y=1}, "5"
//                 {x=2;y=1}, "6"
//                 {x=0;y=2}, "7"
//                 {x=1;y=2}, "8"
//                 {x=2;y=2}, "9"] |> Map.ofList


// let getCode (buttons: Button seq) : string =
//     buttons
//         |> Seq.map (fun button -> keypad.Item button)
//         |> String.concat ""

// let splitLines (lines: string) : string seq =
//     lines.Split(Environment.NewLine.ToCharArray()) |> Seq.ofArray

// let removeBlankLines (lines: string seq) : string seq =
//     lines |> Seq.filter (String.IsNullOrEmpty >> not)

// let parseInstructions (lines: string seq) : Instruction seq seq =
//     let parseLine (line: string) : Instruction seq = line |> Seq.map (fun x -> match x with | 'U' -> Up | 'D' -> Down | 'L' -> Left | 'R' -> Right | _ -> failwith "...")

//     lines |> Seq.map parseLine

// let findButtons (instructions: Instruction seq seq) =
//     let next(button: Button) (instruction: Instruction) =
//         let isValid (button: Button) =
//             if button.x >= 0 && button.x < 3 && button.y >= 0 && button.y < 3
//             then Some button
//             else None

//         let button' = match instruction with
//                         | Up -> { button with y = button.y - 1 }
//                         | Down -> { button with y = button.y + 1 }
//                         | Right -> { button with x = button.x + 1 }
//                         | Left -> { button with x = button.x - 1 }

//         defaultArg (button' |> isValid) button

//     instructions
//             |> Seq.scan (fun x -> x |> Seq.fold next) { x = 1; y = 1}
//             |> Seq.tail


// let run input = input
//                     |> splitLines
//                     |> removeBlankLines
//                     |> parseInstructions
//                     |> findButtons
//                     |> getCode

// // --- Day 3: Squares With Three Sides ---

// open System

// type TriangleDescription = int * int * int

// let splitLines (separator: string) (lines: string) : string seq =
//     lines.Split(separator.ToCharArray()) |> Seq.ofArray

// let removeBlankLines (lines: string seq) : string seq =
//     let trim (str: string) : string = str.Trim()
//     let isNullOrEmpty (str: string) : bool = String.IsNullOrEmpty(str)

//     lines |> Seq.filter (trim >> isNullOrEmpty >> not)

// let parseTriangleDescriptions (lines: string seq) : TriangleDescription seq =
//     let parseLine (line: string) : TriangleDescription = line |> splitLines " " |> removeBlankLines |> Seq.map int |> Seq.sort |> List.ofSeq |> (fun x -> match x with | [a;b;c] -> a, b, c | _ -> failwith "...")

//     lines |> Seq.map parseLine

// let getTriangles (triangleDescriptions: TriangleDescription seq) : TriangleDescription seq =
//     let isTriangle (a: int, b: int, c: int) : bool = a + b > c

//     triangleDescriptions |> Seq.filter isTriangle

// input
//     |> splitLines Environment.NewLine
//     |> removeBlankLines
//     |> parseTriangleDescriptions
//     |> getTriangles
//     |> Seq.length

// --- Day 4: Security Through Obscurity ---

open System
open System.Text.RegularExpressions

type Room = {
    sectorId: int
    name: string
    checksum: string
}

let splitLines (lines: string) : string seq =
    lines.Split(Environment.NewLine.ToCharArray()) |> Seq.ofArray

let removeBlankLines (lines: string seq) : string seq =
    lines |> Seq.filter (String.IsNullOrEmpty >> not)

let (|Room|_|) (room: string) =
    let matching = Regex.Match(room, "^(?<name>[^0-9]*)(?<sectorId>[0-9]*)\[(?<checksum>[a-z]*)\]$")
    if matching.Success then (matching.Groups.["name"].Value, matching.Groups.["sectorId"].Value, matching.Groups.["checksum"].Value) |> Some
    else None

let parseRoom (line: string) : Room =
    match line with
    | Room (name, sectorId, checksum) ->
        {
            sectorId = int sectorId
            name = name.Replace("-","")
            checksum = checksum
        }
    | _ ->
        failwith "..."

let parseRooms (lines: string seq) : Room seq =
    lines |> Seq.map parseRoom

let getRealRooms (rooms: Room seq) : Room seq =
    let isRealRoom (room: Room) : bool =
        let lookup = room.name
                        |> Seq.groupBy id
                        |> Seq.groupBy (fun (k, v) -> v |> Seq.length)
                        |> Seq.sortByDescending (fun (k, v) -> k)
                        |> Seq.map (fun (k, v) -> v |> Seq.map fst)
                        |> List.ofSeq
        room.checksum
            |> Seq.fold (fun (inOrder, index) letter -> inOrder && [0..min index (lookup.Length-1)] |> List.exists (fun x -> lookup.[x] |> Seq.contains letter), index + 1) (true, 0)
            |> fst

    rooms |> Seq.filter isRealRoom

let sumSectorIds (rooms: Room seq) : int =
    rooms |> Seq.sumBy (fun x -> x.sectorId)

let run input = input
                    |> splitLines
                    |> removeBlankLines
                    |> parseRooms
                    |> getRealRooms
                    |> sumSectorIds

run "aaaa-bbb-z-y-x-123[abxyz]"
run "a-b-c-d-e-f-g-h-987[abcde]"
run "not-a-real-room-404[oarel]"
run "totally-real-room-200[decoy]"