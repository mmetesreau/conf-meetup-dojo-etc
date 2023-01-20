open System

type Hour = int
type Name = string

type PartOfTheDay = Dia | Tarde | Noche

let partOfTheDayOf (hour: Hour) = 
    match hour with
    | hour when hour >= 0 && hour < 6 -> Some Noche
    | hour when hour >= 6 && hour < 12 -> Some Dia
    | hour when hour >= 12 && hour < 20 -> Some Tarde
    | hour when hour >= 20 && hour < 24 -> Some Noche
    | _ -> None

type Reversed = StringReversed of string | PalindromeReversed of string 

let reverse input = 
    let reversedInput = 
        input
            |> Seq.rev
            |> string
    
    if input = reversedInput then PalindromeReversed reversedInput
    else StringReversed reversedInput

type Cmd = Start of Name * Hour | Reverse of string | Stop

type Event = 
    | UserGreeted of Name * PartOfTheDay
    | InputReversed of Reversed
    | ProgramStopped of Name

type State = NotStarted | Started of Name | Stopped
let evolve state = function
    | UserGreeted (name, _) -> Started name
    | ProgramStopped name -> Stopped
    | _ -> state

let handle state cmd = 
    match state, cmd with
    | NotStarted, Start (name, hour)->
        match partOfTheDayOf hour with
        | Some partOfTheDay -> [ UserGreeted (name, partOfTheDay) ]
        | None  -> []
    | Started name, Stop -> [ ProgramStopped name ]
    | Started _, Reverse input -> 
        [ InputReversed (reverse input) ]
    | _ -> []

type Message = Cmd * AsyncReplyChannel<Event list>

let startOhce name hour = MailboxProcessor<Message>.Start(fun inbox->

    let rec run (history: Event list) = async {
        let! (cmd, channel)  = inbox.Receive()
        
        let state = List.fold evolve NotStarted history
        let events = handle state cmd
        
        channel.Reply events

        return! run (history @ events)
    }

    let events = handle NotStarted (Start (name, hour))
    
    run events)

let printer (printLine: string -> unit) = function
    | UserGreeted (name, partOfTheDay) -> 
        match partOfTheDay with
        | Dia -> printLine ""
        | Tarde -> printLine ""
        | Noche -> printLine ""
    | ProgramStopped name ->
        printLine ""
    | InputReversed inputReversed ->
        match inputReversed with 
        | StringReversed inputReversed ->
            printLine ""
        | PalindromeReversed inputReversed 
            print ""
            print ""
            
let runConsole () = 
    let agent = startOhce "Mickael" DateTime.Now.Hour
    agent.






















let greet hour name = 
    match hour with
    | Dia -> $"¡Buenos días {name}!"
    | Tarde -> $"¡Buenas tardes {name}!"
    | Noche -> $"¡Buenas noches {name}!"



type readLine = unit -> string
type writeLine = string -> unit
type getHour = unit -> DateTime

type Infra = {
    readLine:readLine
    writeLine:writeLine
    getHour:getHour
}

let run () = 





let handle input = 
    match input with 
    | "Stop!" -> $"Adios {name}"












let run infra = 

let ohce hour name = 
    MailboxProcessor.Start(fun inbox ->
        greet hour name

        let rec run () = async {
            let! msg = inbox.Receive()

            return! run
        }

        run ())
