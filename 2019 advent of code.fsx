open System

let split (separator: char) (str: string) = str.Split(separator)

let join (separator: char) (lst: string seq)= String.Join(separator, lst)

// --- Day 1: The Tyranny of the Rocket Equation ---

module Day1 = 
    let massToFuel mass =
       Math.Floor(mass / 3.) - 2.

    let modules = [
        144475
        145308
        100615
        56900
        128773
        65519
        74165
        99081
        141047
        149128
        148282
        109528
        55909
        70885
        115049
        149631
        52276
        101944
        113005
        102876
        64365
        71178
        122767
        86272
        139199
        78631
        71958
        81288
        70401
        77582
        118275
        115648
        91350
        121735
        130339
        55146
        137351
        101940
        112657
        133288
        81503
        136812
        67015
        142573
        125537
        99231
        61693
        85719
        80659
        148431
        101176
        77853
        108201
        138945
        81804
        55795
        141837
        113490
        57932
        81023
        76756
        79023
        73527
        75874
        63332
        62055
        76124
        54254
        68482
        141113
        84335
        58747
        84723
        137564
        132605
        94970
        50312
        89127
        143858
        124587
        52272
        138039
        53782
        93085
        83456
        94432
        121481
        93700
        114222
        117849
        147460
        110324
        75337
        130464
        88805
        109489
        71109
        95625
        115832
        123252
    ] 

    modules 
        |> List.sumBy (float >> massToFuel)
        |> printfn "result: %f"

    // --- Part Two ---

    let massToFuel' mass =
        let rec massToFuel'' mass totalFuel =
            let fuel = massToFuel mass
            if fuel < 0. then totalFuel
            else totalFuel + fuel |> massToFuel'' fuel

        massToFuel'' mass 0.

    modules
        |> List.sumBy (float >> massToFuel')
        |> printfn "result: %f"

// --- Day 2: 1202 Program Alarm ---

module Day2 = 
    type Opcodes = 
        | Add of int * int * int
        | Multiply of int * int * int
        | Halt

    let read (memory: int array)  (offset: int) =
        match memory.[offset] with
        | 1 -> (memory.[offset + 1], memory.[offset + 2], memory.[offset + 3]) |> Add
        | 2 -> (memory.[offset + 1], memory.[offset + 2], memory.[offset + 3]) |> Multiply 
        | 99 -> Halt
        | _ -> failwith "oups"

    let execute (memory: int array) =
        let step = 4
        let rec process' (memory: int array) (offset: int) =
            match read memory offset with
            | Halt -> memory
            | Add (i1, i2, dest) -> 
                memory.[dest] <- memory.[i1] + memory.[i2]
                offset + step |> process' memory 
            | Multiply (i1, i2, dest) -> 
                memory.[dest] <- memory.[i1] * memory.[i2]
                offset + step |> process' memory         

        process' memory 0

    let load (program: string) =
        program
            |> split ','
            |> Array.map int

    let dumpOutput (memory: int array) =
        memory
            |> Array.head
            |> string

    let program = "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,2,6,23,27,1,6,27,31,2,31,9,35,1,35,6,39,1,10,39,43,2,9,43,47,1,5,47,51,2,51,6,55,1,5,55,59,2,13,59,63,1,63,5,67,2,67,13,71,1,71,9,75,1,75,6,79,2,79,6,83,1,83,5,87,2,87,9,91,2,9,91,95,1,5,95,99,2,99,13,103,1,103,5,107,1,2,107,111,1,111,5,0,99,2,14,0,0"
     
    program 
        |> load
        |> execute
        |> dumpOutput
        |> printfn "result: %s"

    // --- Part Two ---

    let init (noun: int) (verb: int) (memory: int array) = 
        memory.[1] <- noun
        memory.[2] <- verb
        memory

    for noun in [0..99] do
        for verb in [0..99] do
            let output = program 
                            |> load
                            |> init noun verb
                            |> execute
                            |> dumpOutput

            if output = "19690720" then
                printfn "noun: %i" noun                
                printfn "verb: %i" verb                
                printfn "result: %i" (100 * noun + verb)

// --- Day 3: Crossed Wires ---

module Day3 = 
    let centralPort = (0, 0), 0

    type Directions =
        | Up 
        | Down
        | Left
        | Right

    let parseInstruction (instruction: string) = 
        let d::v = instruction |> List.ofSeq

        let value = v |> String.Concat |> int

        let direction =
            match d with 
            | 'U' -> Up 
            | 'D' -> Down
            | 'L' -> Left
            | 'R' -> Right
            | _ -> failwith "oups"

        [1..value] |> List.map (fun _ -> direction)

    let parsePath (path: string) = 
        let apply ((x, y), _) instruction =
            let x', y' =
                match instruction with
                | Up -> x, y + 1
                | Down -> x, y - 1
                | Left -> x - 1, y
                | Right -> x + 1, y

            (x', y'), (abs x' + abs y')    

        path 
            |> split ','
            |> Seq.collect parseInstruction
            |> Seq.scan apply centralPort
            |> Set.ofSeq

    let paths = [
        "R992,U284,L447,D597,R888,D327,R949,U520,R27,U555,L144,D284,R538,U249,R323,U297,R136,U838,L704,D621,R488,U856,R301,U539,L701,U363,R611,D94,L734,D560,L414,U890,R236,D699,L384,D452,R702,D637,L164,U410,R649,U901,L910,D595,R339,D346,R959,U777,R218,D667,R534,D762,R484,D914,L25,U959,R984,D922,R612,U999,L169,D599,L604,D357,L217,D327,L730,D949,L565,D332,L114,D512,R460,D495,L187,D697,R313,U319,L8,D915,L518,D513,R738,U9,R137,U542,L188,U440,R576,D307,R734,U58,R285,D401,R166,U156,L859,U132,L10,U753,L933,U915,R459,D50,R231,D166,L253,U844,R585,D871,L799,U53,R785,U336,R622,D108,R555,D918,L217,D668,L220,U738,L997,D998,R964,D456,L54,U930,R985,D244,L613,D116,L994,D20,R949,D245,L704,D564,L210,D13,R998,U951,L482,U579,L793,U680,L285,U770,L975,D54,R79,U613,L907,U467,L256,D783,R883,U810,R409,D508,L898,D286,L40,U741,L759,D549,R210,U411,R638,D643,L784,U538,L739,U771,L773,U491,L303,D425,L891,U182,R412,U951,L381,U501,R482,D625,R870,D320,L464,U555,R566,D781,L540,D754,L211,U73,L321,D869,R994,D177,R496,U383,R911,U819,L651,D774,L591,U666,L883,U767,R232,U822,L499,U44,L45,U873,L98,D487,L47,U803,R855,U256,R567,D88,R138,D678,L37,U38,R783,U569,L646,D261,L597,U275,L527,U48,R433,D324,L631,D160,L145,D128,R894,U223,R664,U510,R756,D700,R297,D361,R837,U996,L769,U813,L477,U420,L172,U482,R891,D379,L329,U55,R284,U155,L816,U659,L671,U996,R997,U252,R514,D718,L661,D625,R910,D960,L39,U610,R853,U859,R174,U215,L603,U745,L587,D736,R365,U78,R306,U158,L813,U885,R558,U631,L110,D232,L519,D366,R909,D10,R294"
        "L1001,D833,L855,D123,R36,U295,L319,D700,L164,U576,L68,D757,R192,D738,L640,D660,R940,D778,R888,U772,R771,U900,L188,D464,L572,U184,R889,D991,L961,U751,R560,D490,L887,D748,R37,U910,L424,D401,L385,U415,L929,U193,R710,D855,L596,D323,L966,D505,L422,D139,L108,D135,R737,U176,R538,D173,R21,D951,R949,D61,L343,U704,R127,U468,L240,D834,L858,D127,R328,D863,R329,U477,R131,U864,R997,D38,R418,U611,R28,U705,R148,D414,R786,U264,L785,D650,R201,D250,R528,D910,R670,U309,L658,U190,R704,U21,R288,D7,R930,U62,R782,U621,R328,D725,R305,U700,R494,D137,R969,U142,L867,U577,R300,U162,L13,D698,R333,U865,R941,U796,L60,U902,L784,U832,R78,D578,R196,D390,R728,D922,R858,D994,L457,U547,R238,D345,R329,D498,R873,D212,R501,U474,L657,U910,L335,U133,R213,U417,R698,U829,L2,U704,L273,D83,R231,D247,R675,D23,L692,D472,L325,D659,L408,U746,L715,U395,L596,U296,R52,D849,L713,U815,R684,D551,L319,U768,R176,D182,R557,U731,R314,D543,L9,D256,R38,D809,L567,D332,R375,D572,R81,D479,L71,U968,L831,D247,R989,U390,R463,D576,R740,D539,R488,U367,L596,U375,L763,D824,R70,U448,R979,D977,L744,D379,R488,D671,L516,D334,L542,U517,L488,D390,L713,D932,L28,U924,L448,D229,L488,D501,R19,D910,L979,D411,R711,D824,L973,U291,R794,D485,R208,U370,R655,U450,L40,D804,L374,D671,R962,D829,L209,U111,L84,D876,L832,D747,L733,D560,L702,D972,R188,U817,L111,U26,L492,U485,L71,D59,L269,D870,L152,U539,R65,D918,L932,D260,L485,U77,L699,U254,R924,U643,L264,U96,R395,D917,R360,U354,R101,D682,R854,U450,L376,D378,R872,D311,L881,U630,R77,D766,R672"
    ]

    paths
        |> Seq.map parsePath
        |> Set.ofSeq
        |> Set.intersectMany 
        |> Seq.sortBy (fun (position, distance) -> distance)
        |> Seq.iter (fun (position, distance) -> printfn "position: %A distance: %i" position distance)
      
        // --- Part Two ---    

    let parsePath' (path: string) = 
        let apply ((x, y), step) instruction =
            let x', y' =
                match instruction with
                | Up -> x, y + 1
                | Down -> x, y - 1
                | Left -> x - 1, y
                | Right -> x + 1, y

            (x', y'), (step + 1)    

        path 
            |> split ','
            |> Seq.collect parseInstruction
            |> Seq.scan apply centralPort
            |> Set.ofSeq

    let intersect (seq1: ((int * int) * int) seq) (seq2: ((int * int) * int) seq) =
        let map1 = 
            seq1 
                |> Seq.groupBy (fun (position, _) -> position)
                |> Seq.map (fun (position, values) -> position, values |> Seq.map snd |> Seq.min)
                |> Map.ofSeq

        let map2 = 
            seq2
                |> Seq.groupBy (fun (position, _) -> position)
                |> Seq.map (fun (position, values) -> position, values |> Seq.map snd |> Seq.min)
                |> Map.ofSeq
        map1 
            |> Map.filter (fun position _ -> map2 |> Map.containsKey position)
            |> Map.map (fun position step -> step + map2.[position])  
            |> Map.toSeq
            |> Set.ofSeq    

    paths
        |> Seq.map parsePath'
        |> Seq.reduce intersect
        |> Seq.sortBy (fun (position, steps) -> steps)
        |> Seq.iter (fun (position, steps) -> printfn "position: %A steps: %i" position steps)

//--- Day 4: Secure Container ---

module Day4 = 
    let neverDecrease (password:int) =
        password 
            |> string        
            |> Seq.pairwise
            |> Seq.fold (fun s (d1, d2) -> s && (int d1) <= (int d2)) true

    let containsDouble (password:int) =
        password 
            |> string
            |> Seq.groupBy id 
            |> Seq.exists (fun (_, n) -> n |> Seq.length >= 2)

    let containsDouble' (password:int) =
        password 
            |> string
            |> Seq.groupBy id 
            |> Seq.exists (fun (_, n) -> n |> Seq.length = 2)        

    [168630..718098] 
        |> List.filter(fun password -> password |> neverDecrease)
        |> List.filter(fun password -> password |> containsDouble)
        |> List.length

    // --- Part Two ---

    [168630..718098] 
        |> List.filter(fun password -> password |> neverDecrease)
        |> List.filter(fun password -> password |> containsDouble')
        |> List.length

// --- Day 5: Sunny with a Chance of Asteroids ---

//module Day5
type Opcodes = 
        | Add of int * int * int
        | Multiply of int * int * int
        | Print of int
        | Write of int 
        | JumpIfTrue of int * int
        | JumpIfFalse of int * int
        | LessThan of int * int * int
        | Equals of int * int * int
        | Halt

let offset = function
    | Add _ ->  4
    | Multiply _ -> 4
    | Write _ ->  2
    | Print _ ->  2
    | JumpIfTrue _ -> 3
    | JumpIfFalse _ -> 3
    | LessThan _ -> 4
    | Equals _ -> 4
    | Halt -> 0

type Modes = Immediate | Position

let mode (mode: char option) = 
    match mode with
    | Some '1' -> Immediate
    | _ -> Position

let parameter (memory: int array) (mode: Modes) (parameterPointer: int) =
    let parameter = memory.[parameterPointer]
    match mode with
    | Position -> memory.[parameter]
    | _ -> parameter

let instruction (entry: int) =
    let instruction = string entry 
    if instruction.Length <= 2 then
        Seq.empty , int instruction
    else
        instruction.Substring(0,instruction.Length - 2) |> Seq.rev, instruction.Substring(instruction.Length - 2) |> int

let (|AddInstruction|_|) (parameterMode, opcode) =
    if opcode = 1 then
         Some (parameterMode |> Seq.tryItem 0 |> mode, parameterMode |> Seq.tryItem 1 |> mode, Immediate)
    else None     

let (|MultiplyInstruction|_|) (parameterMode, opcode) =
    if opcode = 2 then
         Some (parameterMode |> Seq.tryItem 0 |> mode, parameterMode |> Seq.tryItem 1 |> mode, Immediate)
    else None            

let (|WriteInstruction|_|) (_, opcode) =
    if opcode = 3 then
         Some Immediate
    else None 

let (|PrintInstruction|_|) (parameterMode, opcode) =
    if opcode = 4 then
         Some  (parameterMode |> Seq.tryItem 0 |> mode)
    else None 

let (|JumpIfTrueInstruction|_|) (parameterMode, opcode) =
    if opcode = 5 then
         Some (parameterMode |> Seq.tryItem 0 |> mode, parameterMode |> Seq.tryItem 1 |> mode)
    else None 

let (|JumpIfFalseInstruction|_|) (parameterMode, opcode) =
    if opcode = 6 then
         Some (parameterMode |> Seq.tryItem 0 |> mode, parameterMode |> Seq.tryItem 1 |> mode)
    else None 

let (|LessThanInstruction|_|) (parameterMode, opcode) =
    if opcode = 7 then
         Some (parameterMode |> Seq.tryItem 0 |> mode, parameterMode |> Seq.tryItem 1 |> mode, Immediate)
    else None 

let (|EqualsInstruction|_|) (parameterMode, opcode) =
    if opcode = 8 then
         Some (parameterMode |> Seq.tryItem 0 |> mode, parameterMode |> Seq.tryItem 1 |> mode, Immediate)
    else None

let (|HaltInstruction|_|) (_, opcode) =
    if opcode = 99 then
         Some true
    else None

let read (memory: int array)  (instructionPointer: int) =
    match memory.[instructionPointer] |> instruction  with
    | AddInstruction (pm1, pm2, pm3) ->
        (parameter memory pm1 (instructionPointer+1), parameter memory pm2 (instructionPointer+2), parameter memory pm3 (instructionPointer+3)) 
            |> Add
    | MultiplyInstruction (pm1, pm2, pm3) ->
        (parameter memory pm1 (instructionPointer+1), parameter memory pm2 (instructionPointer+2), parameter memory pm3 (instructionPointer+3)) 
            |> Multiply
    | WriteInstruction pm1 ->
        parameter memory pm1 (instructionPointer+1) 
            |> Write
    | PrintInstruction pm1 ->
        parameter memory pm1 (instructionPointer+1) 
            |> Print
     | JumpIfTrueInstruction (pm1, pm2) ->
        (parameter memory pm1 (instructionPointer+1), parameter memory pm2 (instructionPointer+2)) 
            |> JumpIfTrue
     | JumpIfFalseInstruction (pm1, pm2) ->
        (parameter memory pm1 (instructionPointer+1), parameter memory pm2 (instructionPointer+2)) 
            |> JumpIfFalse
     | LessThanInstruction (pm1, pm2, pm3) ->
        (parameter memory pm1 (instructionPointer+1), parameter memory pm2 (instructionPointer+2), parameter memory pm3 (instructionPointer+3)) 
            |> LessThan
     | EqualsInstruction (pm1, pm2, pm3) ->
        (parameter memory pm1 (instructionPointer+1), parameter memory pm2 (instructionPointer+2), parameter memory pm3 (instructionPointer+3)) 
            |> Equals
    | HaltInstruction _ ->
        Halt
    | cmd -> failwith <| sprintf "oups %A" cmd

let execute (memory: int array) =
    let rec process' (memory: int array) (instructionPointer: int) =
        let instruction = read memory instructionPointer
        let step = offset instruction
        
        // printfn "instructionPointer %i" instructionPointer
        // printfn "instruction %A" instruction
        // printfn "step %i" step

        match instruction with
        | Halt -> memory
        | Add (i1, i2, dest) -> 
            memory.[dest] <- i1 + i2
            instructionPointer + step |> process' memory 
        | Multiply (i1, i2, dest) -> 
            memory.[dest] <- i1 * i2
            instructionPointer + step |> process' memory
        | Print i1 -> 
            printfn "%i" i1
            instructionPointer + step |> process' memory 
        | Write i1 -> 
            memory.[i1] <- Console.ReadLine() |> int
            instructionPointer + step |> process' memory 
        | JumpIfTrue (i1, i2) -> 
            printfn "jumpiftrue %i %i" i1 i2
            if i1 = 0 then
                instructionPointer + step |> process' memory 
            else
                i2 |> process' memory 
        | JumpIfFalse (i1, i2) -> 
            printfn "jumpiffalse %i %i" i1 i2

            if i1 = 0 then
                i2 |> process' memory 
            else
                instructionPointer + step |> process' memory 
        | LessThan  (i1, i2, dest) -> 
            memory.[dest] <- if i1 < i2 then 1 else 0
            instructionPointer + step |> process' memory 
        | Equals (i1, i2, dest) -> 
            memory.[dest] <- if i1 = i2 then 1 else 0
            instructionPointer + step |> process' memory                                          

    process' memory 0

let load (program: string) =
    program
        |> split ','
        |> Array.map int

let program = "3,225,1,225,6,6,1100,1,238,225,104,0,1101,72,36,225,1101,87,26,225,2,144,13,224,101,-1872,224,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1102,66,61,225,1102,25,49,224,101,-1225,224,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,35,77,224,101,-112,224,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1002,195,30,224,1001,224,-2550,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,1102,30,44,225,1102,24,21,225,1,170,117,224,101,-46,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,1102,63,26,225,102,74,114,224,1001,224,-3256,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,58,22,225,101,13,17,224,101,-100,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1101,85,18,225,1001,44,7,224,101,-68,224,224,4,224,102,8,223,223,1001,224,5,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,226,224,102,2,223,223,1005,224,329,101,1,223,223,8,677,226,224,1002,223,2,223,1005,224,344,1001,223,1,223,1107,677,677,224,102,2,223,223,1005,224,359,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,389,101,1,223,223,8,226,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,226,677,224,1002,223,2,223,1005,224,419,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,434,101,1,223,223,1108,677,226,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,677,224,102,2,223,223,1006,224,464,101,1,223,223,1007,677,226,224,102,2,223,223,1006,224,479,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,509,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,524,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,539,1001,223,1,223,108,226,677,224,1002,223,2,223,1005,224,554,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,569,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,584,101,1,223,223,1008,677,677,224,1002,223,2,223,1005,224,599,1001,223,1,223,107,226,677,224,1002,223,2,223,1005,224,614,1001,223,1,223,1108,226,677,224,102,2,223,223,1006,224,629,101,1,223,223,7,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,659,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,674,101,1,223,223,4,223,99,226"

// program 
//     |> load
//     |> execute
 
 // --- Part Two ---


"3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    |> load 
    |> execute