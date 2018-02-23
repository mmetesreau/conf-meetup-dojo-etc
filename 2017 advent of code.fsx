open System
open System.Text.RegularExpressions

let split (separator: char) (str: string) = str.Split(separator)

let trim (str: string) = str.Trim()

let isEmpty (str: string) = String.IsNullOrEmpty(str)

// --- Day 1: Inverse Captcha ---

// let captcha = "91212129" 
// let mutable solution = 0

// for index in [0..captcha.Length-1] do
//     if captcha.[index] = captcha.[(index+1)%captcha.Length] then
//         let digit = captcha.[index] |> string |> int
//         solution <- digit + solution

// // 9

// // --- Day 2: Corruption Checksum ---

// let spreadsheet = [
//     "5 1 9 5"
//     "7 5 3"
//     "2 4 6 8"
// ]

// let mutable checksum = 0

// for line in spreadsheet do
//     let columns = line |> split ' '
//     let mutable largestValue = Int32.MinValue
//     let mutable smallestValue = Int32.MaxValue
//     for column in columns do
//         let number = column |> string |> int
//         largestValue <- max number largestValue
//         smallestValue <- min number smallestValue
//     checksum <- checksum + largestValue - smallestValue

// // 18

// // --- Day 3: Spiral Memory ---

// type State = {
//     x: int
//     y: int
//     r: int
// }

// let identified = 1024
// let mutable state = { x = 0; y = 0; r = 1 }

// for _ in [1..identified-1] do
//     if state.r > 0 then
//         if state.x < state.r then
//             state <- { state with x = state.x + 1 }
//         else if state.y < state.r then
//             state <- { state with y = state.y + 1 }
//         else 
//             state <- { state with r = -state.r }
    
//     if state.r < 0 then
//         if state.x > state.r then
//             state <- { state with x = state.x - 1 }
//         else if state.y > state.r then
//             state <- { state with y = state.y - 1 }
//         else 
//             state <- { state with r = -state.r + 1; x = state.x + 1 }
            
// // 31

// // --- Day 4: High-Entropy Passphrases ---

// let passphrases = [
//     "aa bb cc dd ee"
//     "aa bb cc dd aa"
//     "aa bb cc dd aaa"
// ]

// let mutable validPassphrases = 0

// for passphrase in passphrases do
//     let words = passphrase |> split ' '
//     let duplicateWords = 
//         words 
//             |> Seq.groupBy id
//             |> Seq.filter (fun (_, group) -> group |> Seq.isEmpty |> not)
//             |> Seq.length 

//     if duplicateWords = 0 then 
//         validPassphrases <- validPassphrases + 1

// // 2

// // --- Day 5: A Maze of Twisty Trampolines, All Alike ---

// let maze = [|0;3;0;1;-3|]

// let mutable steps = 0
// let mutable instruction = 0

// while instruction >= 0 && instruction < maze.Length do
//     let jump = maze.[instruction]
//     maze.[instruction] <- maze.[instruction] + 1
//     steps <- steps + 1
//     instruction <- instruction + jump

// // 5

// // --- Day 6: Memory Reallocation ---

// let banks = [| 0; 2; 7; 0 |] 

// let mutable cycles = 0
// let mutable states = Set.empty

// while states.Contains(banks) |> not do
//     states <- states.Add(banks |> Array.map id)
    
//     let maxBlocks = banks |> Array.max
//     let maxBlocksIndex = banks |> Array.findIndex(fun b -> b = maxBlocks)

//     banks.[maxBlocksIndex] <- 0

//     for inc in [1..maxBlocks] do
//         let index = (maxBlocksIndex + inc) % banks.Length
//         banks.[index] <- banks.[index] + 1

//     cycles <- cycles + 1

// // 5

// // --- Day 7: Recursive Circus ---

// let programsWithChildren =
//     """pbga (66)
// xhth (57)
// ebii (61)
// havc (66)
// ktlj (57)
// fwft (72) -> ktlj, cntj, xhth
// qoyq (66)
// padx (45) -> pbga, havc, qoyq
// tknk (41) -> ugml, padx, fwft
// jptl (61)
// ugml (68) -> gyxo, ebii, jptl
// gyxo (61)
// cntj (57)"""
//         |> split '\n'
//         |> Seq.map (fun l -> Regex.Match(l |> string,"^(?<program>[a-z]+) (.)+ -> (?<children>(.*))$", RegexOptions.IgnoreCase))
//         |> Seq.filter(fun m -> m.Groups.Count > 1)
//         |> Seq.map(fun m -> m.Groups.["program"].Value, m.Groups.["children"].Value |> split ',' |> Array.map trim)

// programsWithChildren |> Seq.fold (fun bottomProgram (program,_) -> if bottomProgram |> isEmpty |> not && programsWithChildren |> Seq.exists (fun (_,children) -> Array.contains program children) then bottomProgram else program) ""

// // tknk

// --- Day 8: I Heard You Like Registers ---

"""b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""
    |> split '\n'
    |> Seq.map (fun l -> Regex.Match(l |> string,"^(?<register>[a-z]+) (?<action>[a-z]+) (?<value>[0-9]+) if (?<a>[a-z]+) (?<comparator>.+) (?<b>[0-9]+)$", RegexOptions.IgnoreCase))
    |> Seq.map (fun m -> m.Groups.["register"].Value, m.Groups.["action"].Value, m.Groups.["a"].Value, m.Groups.["comparator"].Value, m.Groups.["b"].Value)
    |> Seq.fold (fun s r -> s) Set.empty
    |> Set.maxElement

// // 1