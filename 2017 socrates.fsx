open System

type Event = 
    | Moved
    | Turned
    | PaintingStarted
    | PaintingStopped

type Position = {
    X: int
    Y: int
}

type Orientation = 
    | Up
    | Left
    | Right
    | Down

type TurtleState = {
    Orientation: Orientation
    Position: Position
    IsPainting: bool
} 
with 
    static member zero = 
                            { 
                                Orientation = Orientation.Right
                                Position = { X = 0; Y = 0 }
                                IsPainting = false
                            }

let move (turtle: TurtleState) : TurtleState = 
    let position = 
        match turtle.Orientation with
        | Up -> { turtle.Position with Y = turtle.Position.Y - 1 }
        | Right -> { turtle.Position with X = turtle.Position.X + 1 }
        | Down -> { turtle.Position with Y = turtle.Position.Y + 1 }
        | Left -> { turtle.Position with X = turtle.Position.X - 1 }
    { turtle with Position = position}

let turn (turtle: TurtleState) : TurtleState = 
    match turtle.Orientation with 
    | Up -> { turtle with Orientation = Orientation.Right }
    | Right -> { turtle with Orientation = Orientation.Down }
    | Down -> { turtle with Orientation = Orientation.Left }
    | Left -> { turtle with Orientation = Orientation.Up }

let startPainting (turtle: TurtleState) : TurtleState = 
    { turtle with IsPainting = true }

let stopPainting (turtle: TurtleState) : TurtleState =  
    { turtle with IsPainting = false }
    
let apply (turtle: TurtleState) (event: Event) : TurtleState =
    match event with 
    | Moved -> turtle |> move
    | Turned -> turtle |> turn
    | PaintingStarted -> turtle |> startPainting
    | PaintingStopped -> turtle |> stopPainting

let handle (turtle: TurtleState) (command: char) : Event list  =
    match command with 
    | '#' -> [ PaintingStarted; PaintingStopped; Moved ]
    | ' ' -> [ Moved ]
    | '\n' -> [ Turned; Moved; Turned; ] @ [ for _ in 1..turtle.Position.X -> Moved ] @ [ Turned; Turned ]
    | _ -> [ ]

let run (printer: TurtleState -> TurtleState) (turtle: TurtleState) (command: char) : TurtleState =
    let applyAndPrint state = apply state >> printer
    handle turtle command |> List.fold applyAndPrint turtle

let consolePrinter (turtle: TurtleState) = 
    Console.SetCursorPosition(turtle.Position.X, turtle.Position.Y)
    if turtle.IsPainting then
        Console.Write("#")
    else
        ()
    System.Threading.Thread.Sleep(50)
    turtle

let runAndPrint = run consolePrinter

""" 
####  ####  ####  ####  ####  #####  ####  ####    ####  ####
#     #  #  #     #  #  #  #    #    #     #       #     #  #
####  #  #  #     ####  ####    #    ####  ####    ###   ####
   #  #  #  #     # #   #  #    #    #        #    #     # #
####  ####  ####  #  #  #  #    #    ####  ####    #     #  #
""" 
    |> List.ofSeq 
    |> List.fold runAndPrint TurtleState.zero