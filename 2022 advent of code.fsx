// https://adventofcode.com/2022

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Helpers =
    let memoize f =
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

    let isNullOrEmpty (str: string) = 
        String.IsNullOrEmpty(str)
    
    let trim (str: string) =
        str.Trim()
        
    let join (separator: string) (strs: string seq) =
        String.Join(separator, strs)
        
    let substr (start:int) (length: int) (str: string) =
        str.Substring(start, length)
        
    let matchGroups (pattern: string) (str: string) =
        Regex.Match(str, pattern).Groups
        
    let matchValue (pattern: string) (str: string) =
        Regex.Match(str, pattern).Value
        
    let ceil (x: float) = 
        Math.Ceiling(x)

    let readAllLines path =
        File.ReadAllLines(path) |> List.ofArray
    
    let readAllText path =
        File.ReadAllText(path)

open Helpers

module Day6 =
    let example =
        readAllText "./day06-example.txt"

    let input =
        readAllText "./day06-input.txt"

    let rec find (size: int) (input: string) (position: int) (marker: string) =
        if marker.Length = size then
            marker, position
        else
            let marker' =
                if marker.IndexOf(input[0]) <> -1 then
                    $"{marker.Substring(marker.IndexOf(input[0])+1)}{input[0]}"
                else
                    $"{marker}{input[0]}"

            find size (input.Substring(1)) (position + 1) marker'

    module Part1 =
        let run () = find 4 input 0 ""

    module Part2 =
        let run () = find 14 input 0 ""

module Day5 =
    let example =
        readAllText "./day05-example.txt"

    let input =
        readAllText "./day05-input.txt"

    let parseInstructions (text: string) =
        text
        |> split "\r\n"
        |> List.map (matchGroups "^move (?<quantity>\d+) from (?<from>\d+) to (?<dest>\d+)$")
        |> List.map (fun groups -> int groups["from"].Value, int groups["dest"].Value, int groups["quantity"].Value)

    let parseStacks (text: string) =
        let header, stacks =
            split "\n" text
            |> (fun x -> List.last x, List.take (x.Length - 1) x)

        let countCols =
            header |> (matchValue "(\d+) $" >> trim >> int)

        [ for col in [ 0 .. countCols - 1 ] do
              yield
                  stacks
                  |> List.map (substr (col * 4) 3 >> trim)
                  |> List.filter (isNullOrEmpty >> not)
                  |> List.map (replace "[" "" >> replace "]" "")
                  |> List.rev
                  |> Stack<String> ]

    module Part1 =
        let run () =
            let stacks, instructions =
                input
                |> split "\r\n\r\n"
                |> fun [ stacks; instructions ] -> parseStacks stacks, parseInstructions instructions

            for from, dest, quantity in instructions do
                for _ in [ 0 .. quantity - 1 ] do
                    stacks[ dest - 1 ].Push(stacks[ from - 1 ].Pop())

            stacks |> List.map (fun x -> x.Pop()) |> join ""

    module Part2 =
        let run () =
            let stacks, instructions =
                input
                |> split "\r\n\r\n"
                |> fun [ stacks; instructions ] -> parseStacks stacks, parseInstructions instructions

            for from, dest, quantity in instructions do
                [ 0 .. quantity - 1 ]
                |> List.map (fun _ -> stacks[ from - 1 ].Pop())
                |> List.rev
                |> List.iter (fun x -> stacks[ dest - 1 ].Push(x))

            stacks |> List.map (fun x -> x.Pop()) |> join ""

module Day4 =
    let example =
        readAllLines "./day04-example.txt"

    let input =
        readAllLines "./day04-input.txt"

    let parseSectionAssignment (sectionRange: string) =
        let [ start; stop ] = split "-" sectionRange
        int start, int stop

    let parseSectionAssignmentPair (line: string) =
        let [ first; second ] = split "," line
        parseSectionAssignment first, parseSectionAssignment second

    module Part1 =
        let isFullyContains ((start1, stop1), (start2, stop2)) =
            (start1 >= start2 && stop1 <= stop2)
            || (start2 >= start1 && stop2 <= stop1)

        let run () =
            input
            |> List.map parseSectionAssignmentPair
            |> List.filter isFullyContains
            |> List.length

    module Part2 =
        let overlaps ((start1, stop1), (start2, stop2)) =
            (start1 >= start2 && start1 <= stop2)
            || (stop1 <= stop2 && stop1 >= start2)
            || (start2 >= start1 && start2 <= stop1)
            || (stop2 <= stop1 && stop2 >= start1)

        let run () =
            input
            |> List.map parseSectionAssignmentPair
            |> List.filter overlaps
            |> List.length

module Day3 =
    let example =
        readAllLines "./day03-example.txt"

    let input =
        readAllLines "./day03-input.txt"

    let toPriority (itemType: char) =
        if Char.IsLower(itemType) then
            (int) itemType - (int) 'a' + 1
        else
            (int) itemType - (int) 'A' + 27

    let findCommonItemType (items: string seq) =
        items
        |> Seq.head
        |> Seq.where (fun x ->
            items
            |> Seq.tail
            |> Seq.forall (fun item -> item.Contains(x)))
        |> Seq.head

    module Part1 =
        let run () =
            input
            |> List.map (fun line ->
                [ line.Substring(0, line.Length / 2)
                  line.Substring(line.Length / 2) ])
            |> List.map findCommonItemType
            |> List.map toPriority
            |> List.sum

    module Part2 =
        let run () =
            [ for group in [ 0 .. input.Length / 3 - 1 ] ->
                  [ input[(group * 3)]
                    input[(group * 3) + 1]
                    input[(group * 3) + 2] ] ]
            |> List.map findCommonItemType
            |> List.map toPriority
            |> List.sum

module Day2 =
    let example =
        readAllLines "./day02-example.txt"

    let input =
        readAllLines "./day02-input.txt"

    type Hand =
        | Rock
        | Paper
        | Scissors

    type RoundResult =
        | Lose
        | Draw
        | Win

    let getRoundScore (hand: Hand) (roundResult: RoundResult) =
        let handScore =
            match hand with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

        let roundResultScore =
            match roundResult with
            | Lose -> 0
            | Draw -> 3
            | Win -> 6

        handScore + roundResultScore

    let parseOpponentHand (hand: string) =
        match hand with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> failwithf "oups"

    let playRound (opponentHand: Hand) (userHand: Hand) =
        match opponentHand, userHand with
        | Scissors, Rock
        | Paper, Scissors
        | Rock, Paper -> Win
        | (opponentHand, userHand) when opponentHand = userHand -> Draw
        | _ -> Lose

    module Part1 =
        let parseUserHand (hand: string) =
            match hand with
            | "X" -> Rock
            | "Y" -> Paper
            | "Z" -> Scissors
            | _ -> failwithf "oups"

        let run () =
            input
            |> List.map (fun line ->
                let [ leftToken; rightToken ] =
                    split " " line

                let opponentHand =
                    parseOpponentHand leftToken

                let userHand = parseUserHand rightToken

                let roundResult =
                    playRound opponentHand userHand

                getRoundScore userHand roundResult)
            |> List.sum

    module Part2 =
        let parseExpectedRoundResult (expectedRoundResult: string) =
            match expectedRoundResult with
            | "X" -> Lose
            | "Y" -> Draw
            | "Z" -> Win

        let playForLoseAgainst (hand: Hand) =
            match hand with
            | Rock -> Scissors
            | Paper -> Rock
            | Scissors -> Paper

        let playForWinAgainst (hand: Hand) =
            match hand with
            | Rock -> Paper
            | Paper -> Scissors
            | Scissors -> Rock

        let playForDrawAgainst (hand: Hand) = hand

        let run () =
            input
            |> List.map (fun line ->
                let [ leftToken; rightToken ] =
                    split " " line

                let opponentHand =
                    parseOpponentHand leftToken

                let expectedRoundResult =
                    parseExpectedRoundResult rightToken

                let userHand =
                    match expectedRoundResult with
                    | Lose -> playForLoseAgainst opponentHand
                    | Draw -> playForDrawAgainst opponentHand
                    | Win -> playForWinAgainst opponentHand

                getRoundScore userHand expectedRoundResult)
            |> List.sum

module Day1 =
    let example =
        readAllLines "./day01-example.txt"

    let input =
        readAllLines "./day01-input.txt"

    module Part1 =
        let run () =
            input
            |> Seq.fold
                (fun (maxCalories, currentCalories) itemCalorie ->
                    if itemCalorie |> isNullOrEmpty then
                        (max maxCalories currentCalories, 0)
                    else
                        (maxCalories, currentCalories + int itemCalorie))
                (0, 0)
            |> fst

    module Part2 =
        let run () =
            input
            |> Seq.fold
                (fun (allCalories, currentCalories) itemCalorie ->
                    if itemCalorie |> isNullOrEmpty then
                        (currentCalories :: allCalories, 0)
                    else
                        (allCalories, currentCalories + int itemCalorie))
                ([], 0)
            |> fst
            |> List.sortByDescending id
            |> List.take 3
            |> List.sum