#r "nuget: Expecto"

module ParfOfTheDay =
    type ParfOfTheDay = 
        | Dia
        | Tarde
        | Noche

    type HourParsed = ParfOfTheDay of ParfOfTheDay | InvalidPartOfTheDay

    let parfOfTheDayOf hour =
        match hour with
        | hour when hour >= 0 && hour < 6 -> ParfOfTheDay Noche
        | hour when hour >= 6 && hour < 12 -> ParfOfTheDay Dia
        | hour when hour >= 12 && hour < 20 -> ParfOfTheDay Tarde
        | hour when hour >= 20 && hour < 24 -> ParfOfTheDay Noche
        | _ -> InvalidPartOfTheDay

module Ohce =
    let greet name hour  =
        match hour with
        | ParfOfTheDay.Dia -> $"¡Buenos días {name}!"
        | ParfOfTheDay.Tarde -> $"¡Buenas tardes {name}!"
        | ParfOfTheDay.Noche -> $"¡Buenas noches {name}!"

    let reverse (input: string) =
        input
            |> Seq.rev
            |> Seq.map string 
            |> String.concat ""

open Expecto
open ParfOfTheDay
open Ohce


let tests = 
    testList "Ohce" [
        yield! [
            for hour in [24;-1;25] ->
                test $"should be invalid part of the day {hour}" {
                    let isInvalidPartOfTheDay = function
                        | InvalidPartOfTheDay _ -> true
                        | _ -> false

                    Expect.isTrue (parfOfTheDayOf hour |> isInvalidPartOfTheDay) ""
                }]

        yield! [
            for hour in [0..23] ->
                test $"should be valid part of the day {hour}" {
                    let isPartOfTheDay = function
                        | ParfOfTheDay _ -> true
                        | _ -> false

                    Expect.isTrue (parfOfTheDayOf hour |> isPartOfTheDay) ""
                }]

        yield! [ 
            for hour in [7;10;11] -> 
                test $"should be dia by {hour}" {
                    Expect.equal (parfOfTheDayOf hour) (ParfOfTheDay Dia) ""
                }]

        yield! [ 
            for hour in [12;17;19] -> 
                test $"should be tarde by {hour}" {
                    Expect.equal (parfOfTheDayOf hour)  (ParfOfTheDay Tarde) ""
                }]

        yield! [ 
            for hour in [3;5;21] -> 
                test $"should be noche by {hour}" {
                    Expect.equal (parfOfTheDayOf hour)  (ParfOfTheDay Noche) ""
                }]

        test "should greet by dias" {
            Expect.equal (greet "Mickael" Dia) "¡Buenos días Mickael!" ""
        }

        test "should greet by tardes" {
            Expect.equal (greet "Mickael" Tarde) "¡Buenas tardes Mickael!" ""
        }

        test "should greet by noches" {
            Expect.equal (greet "Mickael" Noche) "¡Buenas noches Mickael!" ""
        }

        test "should reverse ohce input string" {
            Expect.equal (reverse "ohce") "echo" ""
        }

        test "should reverse echo input string" {
            Expect.equal (reverse "echo") "ohce" ""
        }
    ]

runTests defaultConfig tests