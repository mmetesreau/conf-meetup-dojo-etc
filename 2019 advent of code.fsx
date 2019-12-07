
[<AutoOpen>]
module Helpers = 
    open System

    let split (separator: char) (str: string) = str.Split(separator)

    let join (separator: char) (lst: string seq)= String.Join(separator, lst)

// --- Day 1: The Tyranny of the Rocket Equation ---

module Day1 = 
    open System

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
    open System

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

module Day5 = 
    open System

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
                if i1 = 0 then
                    instructionPointer + step |> process' memory 
                else
                    i2 |> process' memory 
            | JumpIfFalse (i1, i2) -> 
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

    program    
        |> load 
        |> execute

// --- Day 6: Universal Orbit Map ---
module Day6=
    open System

    let input = [|
        "PQK)Q5S"
        "8QF)BST"
        "7DY)PBP"
        "DJG)PLT"
        "L9D)LBP"
        "SBH)WMD"
        "XJZ)PS9"
        "6Q1)FNM"
        "K2F)9D1"
        "NVZ)YQY"
        "3QM)Q3H"
        "25D)ZJ3"
        "94L)SBS"
        "G6L)C81"
        "Q1J)YJZ"
        "C7C)SW7"
        "9JS)6G3"
        "BRF)K9P"
        "7J4)LVJ"
        "15F)K8J"
        "8DG)67M"
        "PL6)DLZ"
        "XMM)9ZW"
        "WQF)SJK"
        "18K)N2G"
        "5LR)X7Y"
        "K5F)S93"
        "8MQ)CZ1"
        "LJK)4L4"
        "MK3)12J"
        "LYH)48W"
        "3K6)QSX"
        "CQJ)Q9Z"
        "4SC)HHY"
        "P6F)475"
        "LZY)VLS"
        "1VJ)GWW"
        "TL3)HFM"
        "NLZ)ZYG"
        "232)F5L"
        "1Q2)B71"
        "D8S)GHQ"
        "7QJ)TWX"
        "DJL)JTS"
        "LLY)J87"
        "VVK)QPT"
        "XSJ)F5R"
        "DP1)M48"
        "4H4)M4J"
        "FJ7)919"
        "JKB)YFR"
        "YT7)7JM"
        "6PC)5LD"
        "HGC)XXL"
        "BGY)JVW"
        "XMB)44W"
        "C62)87F"
        "XB6)C2G"
        "YFN)67Z"
        "MHK)C7T"
        "V77)4VN"
        "WBR)K9Q"
        "134)VMB"
        "CMQ)MPM"
        "CN2)MYR"
        "Y74)T5W"
        "FSD)JPZ"
        "WBR)L4G"
        "FWL)BCR"
        "D6M)NRK"
        "DHG)PWN"
        "V4H)RZD"
        "KJB)5FR"
        "HXM)FSR"
        "SC8)84G"
        "BWH)7TM"
        "T68)174"
        "PWY)6V9"
        "W7S)W8Q"
        "B7J)Z61"
        "9X3)RC5"
        "S54)JJ2"
        "1N5)FR9"
        "1BB)K2V"
        "CWW)Y74"
        "Q9Z)745"
        "YJY)K4R"
        "XTZ)34J"
        "NHD)5YZ"
        "QH1)R7G"
        "F7T)JL8"
        "G8Y)2NF"
        "G19)3FG"
        "N41)JHR"
        "THR)Q9D"
        "6KC)6T8"
        "K2V)PSX"
        "QDJ)93G"
        "ZC9)RBV"
        "4KG)XYF"
        "4R8)FV5"
        "284)SLM"
        "B14)K2R"
        "71Q)4B1"
        "PJ9)NDK"
        "LV2)G5G"
        "TXW)1MP"
        "Z5W)QS7"
        "8CF)KVQ"
        "5KD)HTM"
        "BYH)1JQ"
        "QYW)VZ8"
        "DNN)C7C"
        "9ZW)C62"
        "9X3)LRY"
        "BN3)1WG"
        "4YV)RTL"
        "X4Y)XTQ"
        "V81)YDX"
        "2SP)QCZ"
        "KDL)TJJ"
        "VQM)8X7"
        "CBM)R29"
        "7C9)R3S"
        "BY8)2BS"
        "36X)VPW"
        "H5H)2LK"
        "MDK)TKK"
        "VT4)SG8"
        "3C7)N5R"
        "4XH)WZM"
        "JS4)KTL"
        "QS7)YLW"
        "S7L)DHG"
        "F5L)3C3"
        "SM9)Q5V"
        "RGK)S3X"
        "VNW)LVD"
        "BN1)RZW"
        "6CJ)876"
        "6CX)X4J"
        "11N)42B"
        "KH2)G18"
        "XN4)TMK"
        "HRT)XH4"
        "7ZC)2DD"
        "CVL)WVJ"
        "4BD)WRR"
        "PG7)6GP"
        "V71)LS7"
        "SYR)JPM"
        "XBL)GPT"
        "6D3)MJW"
        "V4Q)TXZ"
        "XS7)R91"
        "LVJ)PFG"
        "CRV)FTT"
        "D78)JP8"
        "XWL)JS9"
        "527)H4J"
        "TB3)YS5"
        "RZD)K6G"
        "8B5)XXK"
        "KQM)44F"
        "J6G)WY4"
        "2VG)R59"
        "1RD)HW6"
        "VVJ)TL3"
        "1CG)4NF"
        "172)NHK"
        "VDV)DB1"
        "3PN)NNF"
        "RR5)657"
        "CLD)VMK"
        "R54)B4F"
        "KKM)BBH"
        "VR3)YWS"
        "25H)RQX"
        "GR5)3TR"
        "X58)2L7"
        "158)BQ9"
        "TNJ)F5F"
        "X3B)CB3"
        "NJW)9NN"
        "T5P)R8J"
        "CWP)G4X"
        "NGF)C26"
        "LTL)Y6Q"
        "JT2)7J4"
        "PFH)ZL8"
        "S43)X9T"
        "1FZ)KS9"
        "YBV)KGY"
        "VSH)WB8"
        "WJ7)1LB"
        "81C)141"
        "8HG)TPY"
        "NL3)DJC"
        "DT4)8W2"
        "LMW)QKY"
        "71V)HZK"
        "3GH)D8X"
        "7ZD)X26"
        "XCM)WQH"
        "B5G)KJ1"
        "2JX)K85"
        "NN8)668"
        "JB1)YP4"
        "2DT)T46"
        "N23)31T"
        "1MJ)6KL"
        "65V)GKM"
        "L34)G5W"
        "475)H7P"
        "DJL)Q4Z"
        "F45)1W6"
        "2B5)FBT"
        "SN1)F6S"
        "LR7)D1M"
        "K3B)ZYB"
        "7JC)YTV"
        "N4Y)ZZV"
        "JTS)6D8"
        "TSM)3BX"
        "ZWX)YN4"
        "S1L)FJ7"
        "RZN)ZP9"
        "HRZ)5SL"
        "DLZ)T83"
        "5JZ)Q46"
        "KWR)D69"
        "YKC)7C8"
        "TRT)4XH"
        "SG8)2YJ"
        "2ZX)YFG"
        "G3K)GPG"
        "V87)7L3"
        "KVN)62F"
        "CKC)PT8"
        "RC5)QY3"
        "1H2)GLW"
        "M4P)RQN"
        "YRF)8TV"
        "VZ8)35G"
        "ZV9)GD6"
        "NGM)9YW"
        "7MR)BLX"
        "MFJ)GM1"
        "CRG)SQK"
        "ZWP)8SR"
        "1V8)RB5"
        "HLH)Y4P"
        "1SB)WMK"
        "7MT)MLM"
        "X95)VDL"
        "21K)CTD"
        "XLR)YK6"
        "SLH)SPQ"
        "BCR)D8S"
        "BYS)XPM"
        "WLS)18L"
        "R5M)66D"
        "88Q)SAN"
        "K3K)B7B"
        "D75)Y5N"
        "K1N)QTH"
        "BHL)N8L"
        "MH4)2TJ"
        "KSG)77M"
        "COM)NQY"
        "QK8)ZWP"
        "N6T)928"
        "N2R)H5H"
        "64R)ZBR"
        "85N)82Z"
        "X15)MDD"
        "27H)DT2"
        "KDC)ZP3"
        "J5W)WH1"
        "24G)4R8"
        "VDV)92J"
        "V5D)DCG"
        "9R3)ZKC"
        "1HC)3F2"
        "VKW)LH9"
        "6SK)XK6"
        "VYV)Q49"
        "3DM)KFJ"
        "JP8)KRC"
        "RCQ)7VK"
        "7NB)7FG"
        "S2J)PXZ"
        "M4J)VVK"
        "J2K)TGL"
        "R5V)GTT"
        "69C)NJW"
        "295)R6F"
        "SM1)ZQ6"
        "TFT)P22"
        "2DD)53N"
        "KHB)SLH"
        "W6R)3BP"
        "VPW)CNF"
        "J8D)4MB"
        "48W)QDF"
        "5NT)S9X"
        "VF3)M76"
        "7FJ)81Y"
        "D53)VBG"
        "MCZ)8XZ"
        "KZQ)47R"
        "93G)1VJ"
        "L1T)GSR"
        "N36)V3L"
        "7FB)ZXV"
        "PFG)ZTG"
        "5PD)84N"
        "C5P)MYV"
        "P7L)MFB"
        "GLW)8HM"
        "CGQ)SW4"
        "VK1)C3B"
        "ZYG)SS8"
        "MP1)3C7"
        "8W3)YW5"
        "2VZ)Y2M"
        "PPN)8H7"
        "MYR)YCS"
        "CF1)Z9H"
        "8QF)F4T"
        "C4K)H6P"
        "6FF)P8P"
        "JZV)NH9"
        "12B)57P"
        "ZN6)51Q"
        "1NH)KBQ"
        "BKN)467"
        "6TN)XMD"
        "7ZY)WR2"
        "8BG)S7L"
        "9YW)FWX"
        "1D8)94L"
        "XTM)6LN"
        "6L4)KP2"
        "MYH)SB9"
        "4Z6)WX4"
        "ZLQ)5D1"
        "RB5)7PV"
        "QWB)5KD"
        "B2J)VQH"
        "SR4)QBK"
        "PYQ)SBH"
        "ZVT)CZQ"
        "6GD)CNZ"
        "MPY)PCK"
        "JP6)NT6"
        "M4Q)L5M"
        "Q75)7B2"
        "SFD)K6S"
        "YKQ)QJN"
        "GPG)1GW"
        "FFT)1VB"
        "VXT)P2M"
        "3C3)KVN"
        "Y18)X22"
        "9BJ)BV8"
        "J5F)XGJ"
        "B17)WMN"
        "YR9)PZY"
        "2MX)WYY"
        "WYB)HXM"
        "BND)QJ1"
        "QD8)HFV"
        "3MD)6NY"
        "RN6)DQ2"
        "9LH)LZN"
        "T76)WQF"
        "4MB)W3V"
        "QZC)9G1"
        "NKF)Z6B"
        "YX5)79S"
        "WR7)9PQ"
        "XDJ)LLL"
        "H7P)K3X"
        "K78)BFX"
        "BZF)RZQ"
        "N7B)Z66"
        "8Z5)JS4"
        "PSS)JSP"
        "Z1S)72K"
        "Y5N)5LL"
        "X21)3HG"
        "3MB)3GJ"
        "Z8S)XQL"
        "3F5)QHK"
        "GX6)VL7"
        "ZRG)PFH"
        "1F2)P9P"
        "KP3)JP6"
        "8DB)7PY"
        "HW6)DZS"
        "RTL)35B"
        "FNB)13J"
        "8Q4)44S"
        "Y6Q)XDV"
        "X7Y)88Q"
        "J8B)MQP"
        "DH7)WPB"
        "QFH)XJZ"
        "N2N)8G1"
        "JBB)HXV"
        "5T5)XCW"
        "CW8)XTZ"
        "62M)9VH"
        "TMV)JJC"
        "GTD)LD6"
        "B99)JJY"
        "V8C)SFD"
        "RHN)KH2"
        "YGP)6GD"
        "CX9)67W"
        "ZSD)QPZ"
        "K63)5GY"
        "4LT)Q7R"
        "9Q7)CY2"
        "HFX)B5M"
        "FQT)4MN"
        "Q75)GVZ"
        "BZD)HVT"
        "69T)NGY"
        "7HL)M8G"
        "5R7)Z8S"
        "FJ1)VSH"
        "VT4)LFX"
        "YPD)9GS"
        "412)VBN"
        "BH9)R6X"
        "T1C)X11"
        "F1J)8PM"
        "S4M)C6M"
        "KRV)Y5S"
        "2K3)DVP"
        "MV9)YT1"
        "735)G1J"
        "M67)HVK"
        "739)6CJ"
        "SS8)WS8"
        "NW5)H19"
        "WR2)4QW"
        "N36)FD2"
        "KT2)K3Z"
        "1TV)KT2"
        "N2N)7Y3"
        "JP5)GLQ"
        "F2K)X66"
        "SXK)RHH"
        "F2Q)L3H"
        "8VH)M2D"
        "N7Z)8JS"
        "C3M)7RW"
        "1JV)FFW"
        "RBV)D2Y"
        "ZC9)CRV"
        "GY7)G61"
        "6T4)CLK"
        "JTS)M1Z"
        "6C7)2JX"
        "1LB)WZS"
        "57N)5D2"
        "WHK)R5W"
        "QM1)K84"
        "FNM)ZVW"
        "HFV)NGF"
        "QV6)H4H"
        "3XC)Q3S"
        "YP4)8X3"
        "35B)6DK"
        "9FW)Y5D"
        "FMN)3MK"
        "1GW)765"
        "DN6)HFP"
        "B2L)Z7T"
        "J5L)7XZ"
        "9MV)5VW"
        "8JW)QK8"
        "Y79)89T"
        "1T3)DSF"
        "CBQ)3QY"
        "M76)6K4"
        "C7K)VNY"
        "CCX)T1J"
        "P6M)SYR"
        "4B1)ZLM"
        "5FX)V6J"
        "HPC)BJZ"
        "9DS)8VS"
        "QXB)VD9"
        "GJ2)W9X"
        "HPT)5TF"
        "9ZG)HX2"
        "K3X)S2W"
        "7PC)14K"
        "KKM)VKW"
        "P22)N7Z"
        "B2W)L92"
        "9FQ)WLR"
        "5G9)L68"
        "SZZ)5P2"
        "3TB)7RJ"
        "SGT)F2K"
        "SNN)58D"
        "92J)H5Z"
        "MZ7)PZL"
        "RKL)SD9"
        "QM5)VDV"
        "HF5)723"
        "XMD)ZR8"
        "PDD)6TQ"
        "J9N)PG7"
        "81Y)D4M"
        "4CP)GTD"
        "3X4)6K9"
        "WN9)WD2"
        "YJZ)GXQ"
        "JQ5)L47"
        "Z5T)3XC"
        "6NY)S2J"
        "JJY)3T5"
        "66V)1L3"
        "GVQ)XRS"
        "9YL)4PT"
        "Z3Y)DWQ"
        "8J9)2TN"
        "6WH)XP4"
        "59Z)3YN"
        "YHS)XTB"
        "JGR)NFG"
        "KLF)MQ8"
        "GS3)7QW"
        "7SF)VL3"
        "6K4)XF4"
        "MZF)3Z3"
        "M59)3MJ"
        "CB2)8Z5"
        "DC4)NC8"
        "Z4C)ZGR"
        "DJR)BWW"
        "5J6)X5P"
        "WHN)11N"
        "9TZ)7RY"
        "B49)5JH"
        "B1W)NT8"
        "T5W)MZF"
        "DGG)5VN"
        "D69)Z3N"
        "R86)8N8"
        "YLX)H3G"
        "7TM)8D9"
        "RVW)6NN"
        "NGM)9HK"
        "D62)YXY"
        "WLR)ZSJ"
        "SW4)BP4"
        "5DK)1MG"
        "27Y)LC3"
        "Z7X)FFM"
        "Y4P)3L2"
        "S93)412"
        "4NF)7YW"
        "RQ6)9JS"
        "C34)96X"
        "2QJ)W7S"
        "9JG)74P"
        "Z54)PFB"
        "4M4)2ZX"
        "1JQ)DXJ"
        "N9P)Q2G"
        "NDK)MZ7"
        "G5W)BHZ"
        "SLD)PZ5"
        "K9P)KSS"
        "L37)KGD"
        "6P6)F33"
        "L5M)CZM"
        "YZP)SSQ"
        "WQH)TP3"
        "QPZ)ZN6"
        "XGB)34L"
        "MQ8)7MR"
        "4BY)1SB"
        "3C3)PDK"
        "4S3)SCX"
        "F53)4TZ"
        "X4J)FN7"
        "3W6)H7B"
        "SLM)4LT"
        "MXC)ZKV"
        "DVL)QVW"
        "C14)KDL"
        "M2D)8DB"
        "Z88)B3K"
        "BTD)VZF"
        "3BX)58C"
        "JMK)TNV"
        "FW9)7MN"
        "5QJ)PKJ"
        "6P8)859"
        "CNZ)7PR"
        "N58)T4W"
        "6KL)DJR"
        "V7Y)CB2"
        "MDD)D5D"
        "HXV)X84"
        "Z57)H77"
        "6LN)MY7"
        "TQ6)229"
        "CB3)527"
        "D7K)8KM"
        "7L3)DBD"
        "WX4)FQW"
        "8JS)PMT"
        "41V)QFC"
        "XNB)6VN"
        "SJG)7FJ"
        "197)BK2"
        "388)N9P"
        "FG2)3B7"
        "XQX)PYQ"
        "8HM)KMR"
        "F3F)N63"
        "V7S)3WX"
        "L7Y)15F"
        "9ZB)5HQ"
        "L1G)575"
        "85P)BXS"
        "HPH)HRG"
        "7CQ)1V6"
        "Q5W)V7X"
        "HQH)QWB"
        "JGL)7HJ"
        "M48)N4Y"
        "PTF)KGC"
        "Y5S)83S"
        "6FM)RYM"
        "R91)KQM"
        "2TJ)TMV"
        "STM)BKN"
        "PT4)TSM"
        "BXH)SRG"
        "KJ1)XHH"
        "3L6)9HL"
        "GPT)Q5W"
        "ZYB)W1T"
        "44W)4B3"
        "KNG)HHC"
        "VW5)4N7"
        "SRG)X3S"
        "WMD)DLV"
        "TNS)T5P"
        "JYQ)8CW"
        "K88)P4C"
        "W2Q)K78"
        "2QT)GTQ"
        "N78)M11"
        "SY4)WL7"
        "5LH)YWK"
        "SWT)2ZL"
        "FTH)KY3"
        "C75)6L4"
        "ZZJ)7V1"
        "J1C)36X"
        "CW7)MKM"
        "V3L)295"
        "739)BVM"
        "DDT)PCM"
        "KWF)J8D"
        "4QP)PDC"
        "2NH)DZY"
        "9FM)DBM"
        "PSG)P3Y"
        "L89)GWP"
        "DQ2)9LH"
        "PX8)7JD"
        "ZHS)J9C"
        "5QN)47N"
        "YWC)K81"
        "19K)TMC"
        "5VF)G7Z"
        "GTV)D63"
        "QKN)KG3"
        "XS9)JYQ"
        "SJC)TQ6"
        "VZV)XLL"
        "HHC)83B"
        "HL4)4YV"
        "YW7)RYY"
        "KK3)13S"
        "6RJ)Z7X"
        "Z5T)XHW"
        "VL3)N23"
        "K2C)ZRG"
        "VBG)J9Z"
        "8SN)MZP"
        "THR)VXG"
        "WS8)KLF"
        "S82)LY6"
        "2Z4)2T6"
        "ZKH)RZY"
        "LCG)BYS"
        "JHR)9FQ"
        "9FM)ZMH"
        "18L)S9M"
        "2DD)281"
        "5D2)375"
        "Z7T)JL4"
        "2ZL)YRK"
        "PTH)N78"
        "5LX)YWH"
        "G2X)KW8"
        "XY4)QH1"
        "MQ6)NQG"
        "QZR)9ZB"
        "14N)WRF"
        "ZCZ)ZSD"
        "C2L)KN6"
        "KR3)CL4"
        "LJ2)3RD"
        "RPX)PPF"
        "8C3)KDF"
        "S7Z)JTM"
        "QC9)XTP"
        "JZC)KRS"
        "99W)3F5"
        "FFM)H5V"
        "3SC)W3T"
        "FD2)Q4W"
        "C43)8YQ"
        "FTT)RM3"
        "R7F)3H2"
        "HFP)3QM"
        "Z48)VRM"
        "36M)56Q"
        "9Z9)JXX"
        "H32)Q3R"
        "L3H)YZ9"
        "1FM)J4X"
        "MP1)HPT"
        "7Q8)YGP"
        "3TK)T17"
        "JJB)TY8"
        "174)8CF"
        "Z37)W8T"
        "YKP)2DH"
        "RXT)XMB"
        "6MX)FZG"
        "QMH)Q42"
        "2CX)PWT"
        "MQP)CKJ"
        "CRQ)XS1"
        "8SR)TK6"
        "NTC)GY2"
        "XQL)17J"
        "VYB)7Q8"
        "YX9)GL1"
        "3T5)J7H"
        "D43)QCF"
        "2NF)GRW"
        "LZN)YNX"
        "542)YY6"
        "HJ3)FM5"
        "G61)8JV"
        "9M5)HCS"
        "4VB)XLR"
        "SKG)MX4"
        "61F)RGB"
        "WZJ)TLB"
        "W1T)9VN"
        "92Q)PTH"
        "NGP)M67"
        "7F6)NY8"
        "C2P)P17"
        "H1M)R5S"
        "FG2)2Z4"
        "S9M)HRT"
        "K4R)JMK"
        "GZV)KWB"
        "4HN)2DT"
        "8CK)5SY"
        "YDX)Y4J"
        "SKF)D11"
        "6XD)39F"
        "41R)R5M"
        "T4W)KK3"
        "3BP)MYF"
        "RCS)NRH"
        "Q5L)HMP"
        "QCF)YDY"
        "19J)X4Y"
        "N16)VL4"
        "FCZ)PG8"
        "467)H2X"
        "TR5)3TN"
        "9T1)CLD"
        "84W)ZKW"
        "PZ5)7YY"
        "BJF)2GZ"
        "8NB)TC6"
        "4N7)6P8"
        "XGJ)2T8"
        "732)L7H"
        "3BG)H7J"
        "JTD)WXZ"
        "FSR)FMN"
        "8V5)MNP"
        "5SY)16T"
        "ZHY)122"
        "182)J5L"
        "SY9)6FF"
        "CPK)J6J"
        "41Q)R6R"
        "HJ3)8FJ"
        "P93)3MB"
        "5S4)QRV"
        "CZQ)9K5"
        "MYV)ZWQ"
        "JL9)YPD"
        "FVX)7YZ"
        "HX2)D62"
        "BST)CQY"
        "WN2)GPP"
        "KGD)5LX"
        "Z3N)ZHY"
        "QZD)RLT"
        "PCK)Z6W"
        "WY4)RNG"
        "VRM)WJ6"
        "WXC)XL8"
        "VZC)NLD"
        "ZG2)JVK"
        "HZV)F9C"
        "CD4)63F"
        "W9D)CCT"
        "6B6)PQV"
        "CN4)9J6"
        "H4G)FVX"
        "NQP)C9R"
        "R8J)P7B"
        "YY4)SVT"
        "MGG)V1N"
        "KDZ)WR7"
        "Y6P)XFP"
        "K6Q)BV1"
        "7RY)K7Z"
        "QG2)FMY"
        "16D)Y9D"
        "PCN)QV6"
        "NV6)V3S"
        "R6S)SLW"
        "XH6)P6F"
        "KGC)LMH"
        "1B4)57N"
        "PN8)TLT"
        "GM1)1TV"
        "GGN)KJB"
        "X9T)YRD"
        "7PR)6FM"
        "XSJ)VB3"
        "TJJ)LTL"
        "WZS)BX6"
        "Q6L)TPL"
        "CH6)7F1"
        "CXW)SVH"
        "9ZG)99V"
        "SPQ)6J1"
        "3TP)GB2"
        "8CW)RRC"
        "WXD)6D3"
        "3HD)XGB"
        "M31)2NH"
        "NFB)V8C"
        "1MP)HPC"
        "DV8)Z3Y"
        "N94)R7B"
        "XC8)62T"
        "CY4)LCG"
        "J2X)B99"
        "7QW)XNJ"
        "LSZ)W9D"
        "PYK)24G"
        "F5R)4DP"
        "KS9)JP5"
        "7PC)8KX"
        "938)V6V"
        "L3D)74Y"
        "FRG)2T4"
        "MLM)BN3"
        "8Q4)9PD"
        "GY2)ZZJ"
        "6FM)SSG"
        "TC5)DRZ"
        "G8C)KNG"
        "M1Z)C2L"
        "XT4)DQM"
        "GTM)STM"
        "Q5V)LY4"
        "W7Q)2Q3"
        "FX2)TR3"
        "J6J)9PS"
        "Q3R)J1C"
        "GKM)QZR"
        "SJK)4JT"
        "RRW)DT4"
        "7SF)MTF"
        "DNB)1W7"
        "SW7)K9K"
        "VNX)5VF"
        "Q3V)L1T"
        "SFB)JQH"
        "3K6)P7L"
        "WLV)M5Z"
        "1T3)Z5T"
        "8FN)D9W"
        "HVK)SML"
        "97T)GX6"
        "G9B)62G"
        "SCX)9QJ"
        "KDF)Y18"
        "FSZ)8NR"
        "QSX)VVX"
        "T5W)NDM"
        "1TF)6H4"
        "MXC)73N"
        "M9D)MLT"
        "GB2)J9N"
        "FPW)596"
        "16W)2MX"
        "HJJ)X21"
        "VB3)2K3"
        "BWC)JJB"
        "KKL)VYB"
        "KRS)8J9"
        "BFX)SD4"
        "8KX)H4N"
        "J5P)27Q"
        "CB2)B17"
        "P97)1Z3"
        "89N)K7M"
        "18X)LHL"
        "VXG)4Z4"
        "66D)SWH"
        "3BX)4H4"
        "DRZ)HN4"
        "9P7)3GS"
        "G5Z)2VZ"
        "4L4)QS6"
        "3YN)CF1"
        "QPT)66V"
        "LBP)H4G"
        "DWQ)6JQ"
        "3GJ)2YK"
        "MJZ)66L"
        "G7Z)234"
        "YC7)14N"
        "FWX)SY4"
        "1MG)SH2"
        "F7Z)NL3"
        "XB5)7F6"
        "J87)M9D"
        "J9Z)BR3"
        "8S6)WXD"
        "KVQ)B2N"
        "JZS)HFX"
        "6YV)H32"
        "WCJ)SZZ"
        "NFY)ST4"
        "83B)MDN"
        "Y5D)TNS"
        "V8C)G2X"
        "SL3)FL5"
        "Q49)GB7"
        "RQX)VXJ"
        "L9F)DC8"
        "256)Y6X"
        "LZL)VQQ"
        "X8M)CSN"
        "7Y3)RLJ"
        "6JQ)99H"
        "L47)84W"
        "1GD)TC2"
        "JZS)VR3"
        "C9C)M59"
        "1RD)LKV"
        "BP4)ZC9"
        "JP8)TDJ"
        "WMN)SSL"
        "5DQ)44T"
        "BXS)NW8"
        "QGG)RBQ"
        "4YG)X73"
        "WD2)6LB"
        "691)4CM"
        "BCK)PXY"
        "672)ZV9"
        "R6F)VYM"
        "NDM)VHY"
        "MJK)NTX"
        "YJY)5LH"
        "1WV)H1M"
        "R13)1DC"
        "NQG)SY9"
        "42C)LV2"
        "WRR)MJZ"
        "YWH)16D"
        "7N3)YLX"
        "VMB)CXW"
        "PCM)XMT"
        "QJD)KNT"
        "3LD)VF3"
        "C1Y)ZWS"
        "BXK)BT4"
        "SDM)GR5"
        "Z6B)Q3K"
        "QHK)6T5"
        "NC8)F8W"
        "WLD)6MX"
        "VFL)BJF"
        "HQ8)4X8"
        "5DK)XB6"
        "JRJ)158"
        "Y3N)QPV"
        "LKV)FDP"
        "SML)1FZ"
        "9RX)BNF"
        "JXQ)BXK"
        "VPV)2Y8"
        "QDF)C77"
        "145)L8C"
        "CX2)WZJ"
        "K2R)YVM"
        "TC6)VXT"
        "CD5)ZHS"
        "VMQ)NNK"
        "Q46)BQC"
        "VRB)M4P"
        "9ZY)N2N"
        "M4N)2CX"
        "CH2)3YM"
        "LD5)WRT"
        "XHW)PSG"
        "XXK)DZN"
        "B5F)16W"
        "M11)1FM"
        "4Z4)31R"
        "GJ4)Z7B"
        "VHV)KFC"
        "V7W)21P"
        "F4N)5TS"
        "43G)JTD"
        "3L3)KZ2"
        "Z4C)RW7"
        "99H)2TM"
        "LT5)NTC"
        "44F)V9H"
        "CZM)8CK"
        "RD9)CN2"
        "DDV)5J6"
        "T9Z)R86"
        "ZXH)NK6"
        "PLT)Q3V"
        "8PS)T1T"
        "Z43)2QT"
        "YXT)45Q"
        "KK8)4CC"
        "BS9)1DW"
        "LB2)2W5"
        "PS9)5G9"
        "LG8)4PR"
        "HKD)S1L"
        "RGB)MCZ"
        "8TC)S3W"
        "YZ9)NFB"
        "859)X95"
        "XDH)JZC"
        "1DB)72N"
        "WRT)JZS"
        "HGJ)B86"
        "8NM)B22"
        "BDV)B4Q"
        "TYF)GHD"
        "2H2)VLW"
        "4RX)DJL"
        "MHB)BQP"
        "Z9H)WHN"
        "MK7)ZCZ"
        "QNR)2LS"
        "D2Y)RW5"
        "18X)41R"
        "B5Z)HQ8"
        "2LS)VCR"
        "8JP)26P"
        "VLW)RZP"
        "6T4)PKY"
        "BG1)4W8"
        "67Z)LRQ"
        "YLM)1QJ"
        "LVL)7KB"
        "DGQ)48K"
        "Y2M)DHZ"
        "FJN)QW7"
        "3L3)BFH"
        "B4F)FWL"
        "4GV)LB2"
        "HY4)TBC"
        "YY4)2XY"
        "VQH)TKG"
        "K2L)SLC"
        "JMK)5DW"
        "XS1)1X7"
        "1VB)7FB"
        "FK3)85F"
        "K9Q)PBL"
        "Q3S)QMH"
        "TW5)4J7"
        "BW4)SLD"
        "9SZ)7PC"
        "M93)JYM"
        "MQR)6TS"
        "3VR)QJD"
        "SLW)R6Z"
        "BQQ)388"
        "CM8)ZZF"
        "LZN)BWQ"
        "Q5W)NV8"
        "PBL)G9L"
        "LH9)XS7"
        "DJC)6BH"
        "PNJ)B3V"
        "MYF)4Z6"
        "GWP)C4L"
        "MYR)RS4"
        "3QG)MT9"
        "CNF)1Q2"
        "37R)FDW"
        "Q42)N58"
        "KJV)7JC"
        "MGZ)J9B"
        "7P2)ZM1"
        "9J6)DNN"
        "4XF)865"
        "D1K)C4K"
        "LD6)9JG"
        "KSS)1JV"
        "VL7)BN6"
        "7PY)JL6"
        "FZG)SNN"
        "6X6)9SZ"
        "4PR)PH1"
        "3XJ)JZF"
        "Q3R)1WV"
        "NRX)YFN"
        "FJ1)LS3"
        "8JV)XWQ"
        "3H5)P6S"
        "T3X)7DY"
        "7X3)LVL"
        "7F6)KY8"
        "281)2N8"
        "KSP)67T"
        "ST4)1H2"
        "RGK)9X2"
        "PSS)4BY"
        "2N8)XTM"
        "16S)L1R"
        "TY5)18B"
        "61K)8LD"
        "WJ6)M9G"
        "B5N)MZ8"
        "C81)CX9"
        "D6C)P12"
        "13J)FQ4"
        "PD2)J8B"
        "36T)BG3"
        "M8G)S4B"
        "FMY)KNS"
        "51T)JBQ"
        "4QW)TFH"
        "D62)VMQ"
        "TXP)PYK"
        "7RJ)NLH"
        "JXX)YGW"
        "5JF)BQQ"
        "QKN)FP6"
        "K1C)1P9"
        "1SK)ZN4"
        "N63)Z5W"
        "R1R)LQ2"
        "BW4)BZ8"
        "QKY)R1N"
        "6G3)H4D"
        "VHL)T56"
        "DVP)ZVT"
        "YDY)45D"
        "MZ6)W23"
        "VL4)LTF"
        "YFG)T3L"
        "G8M)J3B"
        "FS4)B5Z"
        "12J)VNX"
        "1YY)CNG"
        "88Q)T9K"
        "ZGR)TYF"
        "WHH)P56"
        "PWB)F4N"
        "H3G)6DQ"
        "9PZ)B7F"
        "F5P)1G2"
        "4ZN)N9Q"
        "6PC)69X"
        "ZQ6)VYV"
        "911)DN6"
        "SGT)H9S"
        "K3Z)RMC"
        "JXJ)DCC"
        "3Q7)7R1"
        "5L3)K48"
        "63Y)3K6"
        "11C)1GM"
        "PHW)VSF"
        "XJ3)2WG"
        "WRF)7CQ"
        "DZN)25H"
        "GHQ)99W"
        "Y43)GC1"
        "47N)J6X"
        "Q6D)3FL"
        "9QJ)MK3"
        "GTD)GPF"
        "24W)6PB"
        "PH1)1N5"
        "31T)2VG"
        "MZ8)LX5"
        "4VQ)QNR"
        "4DP)LZL"
        "5GY)6C7"
        "VZT)VVJ"
        "NKJ)FX6"
        "47N)MHW"
        "SVH)71Q"
        "B86)NRX"
        "B4Q)6HK"
        "9SM)PLR"
        "RZB)LJ2"
        "3W6)7WT"
        "KWH)PCC"
        "FGY)43N"
        "S93)K88"
        "KG3)RWR"
        "PPF)RNH"
        "NT8)HP2"
        "M4N)QCW"
        "TYK)L7Y"
        "BN6)81C"
        "QDD)XQ6"
        "H77)6CX"
        "D9W)3BG"
        "F8G)J48"
        "H92)6Q1"
        "P4C)MHK"
        "67W)7TC"
        "XK4)P98"
        "1P7)LJK"
        "1W6)8Q4"
        "KWB)2WX"
        "QPR)4NH"
        "45B)PV2"
        "DNS)DGQ"
        "LMB)64R"
        "JJ2)GJ4"
        "KKZ)T67"
        "C56)P15"
        "XH4)Y6P"
        "VYM)HN1"
        "SW4)11C"
        "8F8)B1Y"
        "PV2)FJ1"
        "C85)R32"
        "XSN)TDR"
        "JBQ)4C4"
        "75K)RKL"
        "BT4)KK8"
        "7YZ)CGQ"
        "X3S)19J"
        "MLT)3QX"
        "BQP)F2Q"
        "HYJ)S6Z"
        "KRX)S8C"
        "NH7)BGW"
        "VZC)S37"
        "XM2)L9F"
        "J7F)4WR"
        "V8Y)5T5"
        "5V2)NVZ"
        "689)H91"
        "YS5)9BJ"
        "JJ2)KDC"
        "26P)DP1"
        "NRH)8KP"
        "NDD)1HC"
        "S5F)KDZ"
        "PC1)YXT"
        "HQY)XYK"
        "745)PKG"
        "375)LKZ"
        "Z9K)2VV"
        "DZY)Y8J"
        "VVX)3YJ"
        "3M3)YDT"
        "8NX)XX4"
        "8FJ)HZG"
        "TNZ)J24"
        "QRV)MPY"
        "L3D)DCS"
        "LFX)8F8"
        "BM1)117"
        "PFB)ZQQ"
        "L7Y)MV9"
        "TKG)6SK"
        "8Z1)2Q9"
        "HB8)LSZ"
        "N78)MFY"
        "MFP)YV7"
        "832)P93"
        "JNX)61F"
        "YLW)K54"
        "8KT)F7Z"
        "DQM)82P"
        "MPM)QR7"
        "V61)TFT"
        "TXZ)HS7"
        "QFD)51T"
        "8G1)CVL"
        "4W8)FS4"
        "KFC)QTY"
        "CQY)PSK"
        "W9X)FFT"
        "X8P)Y43"
        "P94)3XJ"
        "378)4VF"
        "9YS)DJG"
        "PD1)FPW"
        "ZWQ)TYK"
        "3Z3)PD1"
        "B5M)3Q7"
        "GJ5)DNM"
        "LJZ)K63"
        "B3N)MGX"
        "WM3)GL8"
        "K6Q)86R"
        "YNX)CRG"
        "7LG)FY5"
        "K6S)F3F"
        "19J)3DH"
        "PGZ)GJ5"
        "W2S)ZV8"
        "YK6)GS4"
        "9ZX)CM7"
        "HFM)XBL"
        "DSF)ZLQ"
        "VFX)45P"
        "DH1)GGN"
        "V3S)1J3"
        "LC3)DY9"
        "NKF)8GX"
        "VSF)7JZ"
        "TKN)R54"
        "F8T)42C"
        "BBH)KS4"
        "DR8)232"
        "T17)C7K"
        "2DH)B2L"
        "RW1)V77"
        "58D)1MJ"
        "6G5)JKB"
        "DNM)P94"
        "PDT)TC5"
        "7WT)41V"
        "QTH)MH4"
        "3B7)KHJ"
        "KTM)K2C"
        "FDP)XC6"
        "1GM)ST9"
        "QW7)LR7"
        "MYH)KWR"
        "45P)PPN"
        "4KQ)TL5"
        "MZ6)JXM"
        "TM6)NH7"
        "6GP)CYN"
        "PT8)MXC"
        "D8R)1BB"
        "S99)TG8"
        "CH2)CBQ"
        "6T8)B2J"
        "TP3)QDL"
        "KG3)PSS"
        "J24)Q6L"
        "BWW)LG8"
        "YWK)DB8"
        "FDW)1W2"
        "HG7)GZV"
        "9LG)CD5"
        "3HB)QDD"
        "YWS)8KT"
        "ZSY)SH8"
        "XZB)VNW"
        "ZTG)PC1"
        "HWZ)M3S"
        "6ZQ)G5Z"
        "BDC)GV9"
        "B83)FWN"
        "M5L)FSD"
        "23K)9S5"
        "VXJ)7J8"
        "H91)YKC"
        "YT1)HLX"
        "9PQ)L9D"
        "FN7)62M"
        "ZBR)4VQ"
        "8XM)J6G"
        "2V7)2MV"
        "V9H)561"
        "PXY)T6N"
        "WBZ)8CR"
        "P4C)RD9"
        "M5Z)3HB"
        "72K)79K"
        "9NN)FLS"
        "4C4)YX9"
        "LNW)6Z8"
        "CP1)RR5"
        "4CM)KG9"
        "K16)BGY"
        "H5V)C85"
        "PKG)XP7"
        "CCX)L5C"
        "6D8)G6P"
        "V7W)119"
        "371)WPK"
        "P17)KKZ"
        "NTV)MNK"
        "2ZL)8S6"
        "8N8)2MB"
        "159)XS9"
        "SH2)Z54"
        "7MN)79V"
        "48K)2SX"
        "R5Q)7CV"
        "8X7)1PW"
        "4K9)QSW"
        "TPB)7RK"
        "77M)NQS"
        "4CC)5Y2"
        "XDV)SDH"
        "FN7)MHB"
        "VZF)K16"
        "GXG)1SK"
        "FSM)259"
        "H4H)PSZ"
        "WVJ)MP1"
        "3CV)3FB"
        "2DZ)1R4"
        "NF3)HQH"
        "H2X)61M"
        "95X)PN8"
        "6NL)MX5"
        "8RS)YC7"
        "SQK)VW5"
        "372)8Z4"
        "876)W2S"
        "QT4)Z9K"
        "P79)JQF"
        "74Y)SBR"
        "FL3)Q6D"
        "N2G)XNB"
        "447)19K"
        "F5R)WCJ"
        "JSP)HRZ"
        "9HL)C7H"
        "13Q)JL9"
        "HPL)QC9"
        "8VZ)6XD"
        "XNJ)9DS"
        "RH2)BHC"
        "7CV)WKY"
        "NV8)8PG"
        "N9L)RTM"
        "YCF)ZWN"
        "XQX)RW1"
        "G6P)F45"
        "LLL)JTJ"
        "G9L)1DB"
        "R2H)9PT"
        "MFB)S5F"
        "Z61)MDS"
        "74P)XM5"
        "P15)J5P"
        "34L)DQW"
        "QR7)6N4"
        "YJX)5JF"
        "12Q)3HQ"
        "Z6T)PBY"
        "HNL)JQ5"
        "B3K)QHP"
        "BM8)Y79"
        "V7X)ZV6"
        "P3S)QF9"
        "S6L)5QN"
        "VG9)PHW"
        "DHZ)KWF"
        "XG2)4XF"
        "9CK)HQ7"
        "FSP)1J1"
        "51Q)8HG"
        "SBS)BDZ"
        "MHK)XH6"
        "MM9)4K9"
        "7X3)JMQ"
        "BVM)3W6"
        "HZG)DNS"
        "96K)CCY"
        "6CJ)FPX"
        "JZP)Y85"
        "K1L)T6X"
        "ZJX)NKF"
        "8W3)FQT"
        "6T5)VK1"
        "2TM)57T"
        "QBK)R34"
        "GZV)13Q"
        "DB8)PD2"
        "KN6)NNZ"
        "6BF)4HN"
        "RNG)NDD"
        "YFC)CWP"
        "KVS)JJZ"
        "FQ6)691"
        "BLX)J9V"
        "YVM)K6Q"
        "MF6)DVL"
        "QTY)F7T"
        "H9S)G6L"
        "NJK)97T"
        "WLD)PQK"
        "K6G)1TF"
        "7YQ)5HX"
        "Q4Z)XM2"
        "GTQ)5SJ"
        "TDG)X3B"
        "L92)9T3"
        "HN1)QPR"
        "16T)LLY"
        "4NT)SGT"
        "1R4)6KX"
        "VYX)C1X"
        "S3W)1P7"
        "TBC)G7F"
        "LKP)JZV"
        "QSW)XK4"
        "LRY)PT4"
        "RMC)TYD"
        "Q4W)4GV"
        "3L2)9PZ"
        "GPP)NN8"
        "WPK)TVH"
        "LR7)1YY"
        "FQ4)Z37"
        "ZGK)BPR"
        "T67)NSL"
        "RYY)447"
        "KP6)MH2"
        "RS4)6V7"
        "9P8)9F6"
        "XCD)1T3"
        "FBT)PJ9"
        "FX6)CH2"
        "X73)32X"
        "MJ8)6S3"
        "6QP)B1Q"
        "WTD)K79"
        "1PS)XVW"
        "45Q)Q5L"
        "F8W)2SP"
        "3TR)V53"
        "XWQ)JGR"
        "HG7)QD8"
        "R34)JZW"
        "99V)LZQ"
        "JYM)11X"
        "YCS)8MR"
        "JXM)YGS"
        "D1M)HM4"
        "PD3)CY4"
        "FJ7)GGT"
        "SD4)W4M"
        "DY9)TDG"
        "CCL)NMB"
        "1S3)TQP"
        "ZJX)TXP"
        "TQ6)3SH"
        "723)2P1"
        "2WX)Z88"
        "6DK)LJV"
        "8PG)6DN"
        "NGY)8PS"
        "HVB)8RS"
        "NFG)8VH"
        "TC2)C1V"
        "SVT)15M"
        "QVW)XDJ"
        "9LH)VRB"
        "865)NRQ"
        "N38)ZJX"
        "3YM)9ZY"
        "MZJ)MFJ"
        "PBY)RCQ"
        "R6X)5QJ"
        "CD5)36M"
        "8QQ)VPV"
        "5BP)NFY"
        "8KP)Z3G"
        "7CL)NQ1"
        "L3P)1HH"
        "L5C)134"
        "HZ6)PXW"
        "3QY)XDH"
        "B5N)LCR"
        "TDJ)B3N"
        "C7T)WXC"
        "DDR)BRF"
        "J6X)YDP"
        "RRC)89F"
        "XYK)8QS"
        "TMC)159"
        "7JM)QWX"
        "RZY)8MV"
        "913)JB1"
        "N1P)JBZ"
        "W6K)7P2"
        "GJF)SGL"
        "WQ2)97D"
        "Q4Z)96K"
        "NXF)P97"
        "Q57)T2S"
        "MY7)1H1"
        "8XZ)9V3"
        "SML)Q16"
        "WMK)D1K"
        "Z7B)K3B"
        "9XS)MGG"
        "K79)878"
        "N38)K1N"
        "FV5)QXB"
        "QDQ)7ZC"
        "5TS)MQ6"
        "BJZ)2ZR"
        "3QM)8BG"
        "Y4J)LM6"
        "TNV)JRJ"
        "R5S)G74"
        "S9X)89N"
        "3K3)C2P"
        "5LX)PNX"
        "RQN)N2R"
        "MM6)3TK"
        "6DQ)KTC"
        "NW8)1SL"
        "4NH)938"
        "GXG)LWM"
        "QPV)VFX"
        "2D1)RCS"
        "B3S)5PD"
        "RZW)CV2"
        "BG3)8VW"
        "KHJ)735"
        "VMK)K2F"
        "NRX)S43"
        "JJC)KMC"
        "TBC)45B"
        "5DW)7N3"
        "Z66)4SJ"
        "JWF)L89"
        "89T)8ML"
        "DDJ)5GZ"
        "8Z4)8V5"
        "6NK)34V"
        "LS7)H1L"
        "CY2)JKN"
        "S7Z)NZT"
        "4VF)FGC"
        "7RK)B5G"
        "YJX)M93"
        "G4X)9ZZ"
        "PWT)7JF"
        "TQP)XT8"
        "6TQ)9ZC"
        "NBR)5QM"
        "GL1)B7J"
        "G7S)T1C"
        "T9K)DNB"
        "43J)71V"
        "W4M)J7F"
        "Q79)DD8"
        "5JX)6FL"
        "B1Q)SM1"
        "MNP)SV5"
        "FL3)F97"
        "VHY)X8P"
        "9X2)9VR"
        "RZP)VH6"
        "BQC)ZZ3"
        "ZFL)W2Q"
        "2BS)HYH"
        "43N)HQY"
        "97M)3H4"
        "ZKV)GJ2"
        "QCW)N1P"
        "SY4)QG2"
        "G7F)WSX"
        "NLD)18X"
        "4NT)4M4"
        "9V3)WLS"
        "2Y8)3M3"
        "ZP3)C9C"
        "8MR)N9L"
        "4RX)JGL"
        "MNW)59Z"
        "XFP)YBV"
        "MY7)6MB"
        "FM5)718"
        "XFC)HY4"
        "R26)FX2"
        "5VW)9YL"
        "T6N)7SF"
        "WBZ)WFR"
        "MJW)7BS"
        "JL6)N16"
        "57T)J2K"
        "NQ9)WCX"
        "G2M)689"
        "V53)YHS"
        "GPF)HPH"
        "8GX)D43"
        "CDT)R13"
        "HS7)GTM"
        "JZW)KPH"
        "MTF)L3P"
        "8D9)YJC"
        "TDR)5DQ"
        "3FL)797"
        "GSW)NHD"
        "K34)S82"
        "6LB)3CV"
        "MFH)8QQ"
        "GVZ)9P7"
        "CWP)TW5"
        "RCR)B3S"
        "TFS)7L4"
        "8H7)911"
        "Z6L)D7K"
        "K3G)GFB"
        "8PM)MFH"
        "FLS)SKF"
        "XTB)RFV"
        "83S)D3C"
        "WJ7)KSR"
        "3X4)XZB"
        "RW1)PL6"
        "3HQ)S34"
        "CM7)ZT3"
        "X3S)8B5"
        "WTD)6RJ"
        "9YS)RCG"
        "9V7)XVN"
        "N8L)8S4"
        "P5T)VN1"
        "B1Y)L3W"
        "CCY)MCV"
        "FQW)C2W"
        "1RY)88W"
        "NG5)JSS"
        "7JZ)TKD"
        "K1N)94G"
        "Z4J)WF2"
        "B2J)KM3"
        "1Z3)5XX"
        "1PW)B83"
        "QPK)6NK"
        "8NR)LYD"
        "8TC)5G5"
        "8XY)LBT"
        "XVW)7YQ"
        "657)C34"
        "6V9)R7F"
        "TS6)36K"
        "JS4)542"
        "CLK)V8Y"
        "D11)BM1"
        "S6Z)HG7"
        "PD3)R7T"
        "58C)ZJZ"
        "R1N)LMB"
        "63F)WN2"
        "JL4)3H5"
        "53N)5G4"
        "X26)CM2"
        "DB1)MQR"
        "96X)J5F"
        "3MW)XSJ"
        "ZJ3)4RX"
        "7B2)KCD"
        "8QS)KVH"
        "LN4)9XS"
        "X84)9V7"
        "V6J)3L3"
        "VTB)ZKH"
        "2TN)N3G"
        "5XX)PD3"
        "MHW)6YW"
        "W4M)WJ7"
        "ZXV)889"
        "2YJ)MGZ"
        "7F1)2QJ"
        "CGQ)XJQ"
        "XLL)CH6"
        "J4X)LJZ"
        "DC4)R78"
        "9HK)B52"
        "5RP)YOU"
        "79S)XWL"
        "D4M)PCN"
        "H9S)RCY"
        "23Z)3PN"
        "57P)DQF"
        "YFR)8FN"
        "YJC)8LP"
        "BR3)VCS"
        "WQ5)YFC"
        "96X)Z6L"
        "P8P)18K"
        "8BH)V81"
        "67T)HB8"
        "5G5)V61"
        "TW8)9RX"
        "K5K)YJY"
        "3DH)9GC"
        "QCQ)Y35"
        "V7X)2ST"
        "3GS)C1Y"
        "HP2)V5D"
        "VNY)Y1H"
        "NNF)VHV"
        "JQF)J2X"
        "GHD)C75"
        "KM3)7CL"
        "PNX)5C4"
        "4WR)BWZ"
        "C2W)4SC"
        "DCG)8C7"
        "Q3H)S54"
        "L1R)GLM"
        "45D)DZ4"
        "5FR)QGG"
        "YDT)RVW"
        "DP1)YW7"
        "4B3)172"
        "JD9)ZYH"
        "RZQ)MM6"
        "6V7)Q1J"
        "SH8)BYH"
        "K8J)5JZ"
        "3H2)CCX"
        "BK2)X58"
        "8LD)3TB"
        "59Z)B1W"
        "QZC)J5W"
        "PLT)QDJ"
        "LML)QFH"
        "9ZZ)9LL"
        "DLN)GPH"
        "RLT)T9Z"
        "VBN)9KF"
        "QY3)BG1"
        "668)RM2"
        "K7Z)65V"
        "4X8)7RT"
        "Y1H)BPK"
        "SGL)TM6"
        "QS6)XCM"
        "9D1)B2W"
        "Q7Y)11D"
        "VSF)LCK"
        "PXZ)FHP"
        "X11)PZC"
        "B9B)DDJ"
        "JMQ)C56"
        "BZ8)ZFJ"
        "GLM)SFB"
        "ZP9)SKG"
        "9KF)N94"
        "XHH)72S"
        "KCD)6X6"
        "5JH)95X"
        "2CN)23Z"
        "7FG)FPB"
        "J9B)HZV"
        "KZ2)92Q"
        "C62)PS2"
        "ZT3)S6L"
        "F33)FT1"
        "GWR)S99"
        "6C8)Y2G"
        "LWM)NQ9"
        "LY4)CWW"
        "GGT)MXT"
        "G23)P5T"
        "N5R)85P"
        "N3G)K34"
        "MZP)HGJ"
        "P68)YJX"
        "ZFL)YKP"
        "P98)MJ8"
        "FL5)T76"
        "YL4)LV3"
        "MCF)QT4"
        "1PB)PTF"
        "BNF)BH9"
        "JPM)HPL"
        "PYK)KP6"
        "9GC)HF5"
        "JBR)R1R"
        "ZQQ)WX6"
        "MDN)ZWX"
        "R3S)V4H"
        "4LT)FJN"
        "N1P)CPK"
        "P12)G8C"
        "ZV9)25D"
        "Q49)371"
        "BST)TNZ"
        "P3Y)YLM"
        "7RW)CF2"
        "MCV)YR9"
        "7V1)YBD"
        "3RD)8Z1"
        "YY6)JNX"
        "HNX)NJK"
        "1J3)1HM"
        "F97)K3G"
        "VG9)6WH"
        "H19)GPD"
        "JBZ)VQM"
        "CL4)RZB"
        "H3G)69T"
        "6TS)B9B"
        "5B9)8NB"
        "SKG)M31"
        "72S)7ZB"
        "RM2)5C1"
        "J9V)BY8"
        "GFB)46M"
        "YS8)G8M"
        "BFH)NGM"
        "S67)YB4"
        "6ZT)FG2"
        "DZS)YX5"
        "P7B)L67"
        "TMK)XB5"
        "TLB)BZF"
        "BX6)R5V"
        "7HJ)GGB"
        "9FW)F8G"
        "PPM)MBF"
        "GXQ)VZC"
        "ZWN)XNF"
        "CKJ)SXK"
        "GPH)6QM"
        "WMN)9FF"
        "JQH)D78"
        "GWW)1RD"
        "DQW)1B4"
        "KNS)PGZ"
        "4GK)GQ4"
        "C3B)QKN"
        "62G)TWW"
        "HW8)9JB"
        "X5P)182"
        "7L4)HKD"
        "3KQ)DGG"
        "LC3)7X3"
        "GZJ)Z4J"
        "NNK)63Y"
        "LCK)9TZ"
        "Y85)RK6"
        "GGL)8DG"
        "ZMH)3TP"
        "MT9)S5L"
        "BRW)G3K"
        "2VV)JBR"
        "LX5)6RW"
        "PSX)CP1"
        "ZLM)RK8"
        "KBQ)QZC"
        "GC1)8JP"
        "ZL8)D79"
        "S6L)5S4"
        "47R)GGL"
        "LS3)CN4"
        "FHP)N7B"
        "HQ7)CM8"
        "5SJ)XG2"
        "VBN)CDT"
        "V6V)SL3"
        "YW5)5BP"
        "1DV)DDV"
        "D3C)XMM"
        "7F1)V7Y"
        "GQ4)WHK"
        "WKY)32D"
        "CYN)5ZK"
        "4PT)YLH"
        "T1C)HWC"
        "SH5)YL4"
        "5SL)BTD"
        "XPM)TNN"
        "6XD)D75"
        "8KM)YS8"
        "CV2)QCQ"
        "GR5)KP3"
        "ZN6)DDT"
        "YGS)9P8"
        "82Z)WM3"
        "ZX6)PL3"
        "HWZ)1PS"
        "G5G)3GH"
        "C1V)B49"
        "M9G)ZX3"
        "M16)FHF"
        "TVH)MK7"
        "R3S)YCF"
        "9LL)2DZ"
        "PSD)Q7Y"
        "H7J)YWC"
        "NSK)DH7"
        "XF4)6TZ"
        "5Y2)FGY"
        "QJN)1RY"
        "TGL)CX2"
        "SV5)WBZ"
        "RFV)6P6"
        "8CR)RBG"
        "JM8)53M"
        "KY8)SC8"
        "MDK)LML"
        "1H2)G2M"
        "LHL)HJJ"
        "FR9)8BH"
        "T1J)TXW"
        "VG3)256"
        "SBR)WLV"
        "8YJ)NSK"
        "MB9)7B4"
        "CN2)P68"
        "7YY)3L6"
        "LKZ)Z1S"
        "58C)WHH"
        "YDP)284"
        "4BY)BRW"
        "R6S)Y5W"
        "575)3DM"
        "L9F)YT7"
        "79K)6C8"
        "66L)NPC"
        "1DC)9Z9"
        "44T)NTV"
        "6MB)NFD"
        "2T6)DZM"
        "LM6)DLS"
        "W3Y)V7S"
        "1QJ)SN1"
        "NDK)4CP"
        "TPY)K1C"
        "X3J)8W3"
        "PMT)FJQ"
        "YRK)F5P"
        "34J)8YP"
        "LTF)3HD"
        "5YZ)Z8P"
        "2ZR)7B3"
        "69X)W7Q"
        "2SX)WDB"
        "CPK)GXG"
        "FPB)ZGK"
        "2ST)N38"
        "VQQ)TNJ"
        "8YQ)12Q"
        "PZL)V71"
        "FGC)D6M"
        "K5Z)7PP"
        "8V2)NXF"
        "5G4)NLZ"
        "RW7)MDK"
        "PKJ)Z4C"
        "R7T)G7S"
        "1VJ)H92"
        "QXH)P79"
        "36X)X8H"
        "NQY)XC8"
        "39F)NBR"
        "4J7)P6M"
        "19Z)JD9"
        "R5M)8V2"
        "TYD)F1J"
        "KSR)N36"
        "MGG)16Z"
        "ZZV)BND"
        "2L7)3KH"
        "2YS)QXH"
        "5C4)QM1"
        "6YW)JLX"
        "6DN)WXW"
        "CCT)L85"
        "SSQ)378"
        "13S)VHL"
        "1W7)X8M"
        "YRD)BKW"
        "VXJ)DQR"
        "W3V)61K"
        "6CQ)MFP"
        "7C9)ZPD"
        "4ZX)LZB"
        "PPF)HYJ"
        "CZY)DR8"
        "B71)JXJ"
        "Y2G)GZJ"
        "S34)FTW"
        "YB4)Z57"
        "LVD)FL3"
        "4SJ)7DR"
        "YXY)B5N"
        "PNK)CW8"
        "WCX)RXT"
        "HWC)151"
        "7DR)34X"
        "R7B)XMQ"
        "LV3)SVS"
        "G1J)QM5"
        "2Q3)GVQ"
        "Q42)TPB"
        "V7Y)QDQ"
        "117)6ZQ"
        "LMH)DJZ"
        "G8C)6TN"
        "2XB)5V2"
        "R2H)1CG"
        "7JD)85N"
        "NZT)T3S"
        "2R1)YDD"
        "34X)8MQ"
        "Z53)Q57"
        "F5F)VZV"
        "1J1)KJV"
        "87F)B14"
        "HZK)FLP"
        "8S4)DB6"
        "GWR)WTD"
        "X26)1GD"
        "74P)BS9"
        "S3W)YKQ"
        "H4D)R5Q"
        "5C1)6KC"
        "DBM)8JW"
        "928)4BL"
        "TWX)NGP"
        "Q9D)KR3"
        "DBD)LMW"
        "TY8)4XW"
        "LRQ)7ZY"
        "JTM)DDR"
        "VBZ)Y3N"
        "PZY)SJC"
        "151)8X6"
        "CZY)R6S"
        "XCW)3SC"
        "J48)JBB"
        "H4N)9GM"
        "MXT)QYW"
        "D4Q)2M7"
        "R7G)7HL"
        "MH2)CW7"
        "SYR)D8R"
        "NQP)BCK"
        "1LB)46F"
        "DLV)3YK"
        "WDB)3X4"
        "PZC)5SR"
        "TLT)P3S"
        "B99)1JS"
        "XMT)V7W"
        "S2W)5JX"
        "4WB)27Y"
        "3FB)145"
        "878)Z53"
        "G74)XMR"
        "8X6)2PN"
        "Y6X)9ZX"
        "7C8)W66"
        "B22)FRJ"
        "SBR)QPK"
        "7B3)DJS"
        "W3T)GY7"
        "SSL)K5K"
        "K5F)BN1"
        "S5L)VT4"
        "LZQ)9W7"
        "234)4GK"
        "5SR)SG6"
        "ZN4)YRF"
        "46F)D53"
        "BKW)CD4"
        "NRQ)ZX6"
        "S8J)2QN"
        "WF2)VZT"
        "BHZ)2V7"
        "L4G)FLJ"
        "K7M)3VR"
        "W6R)W3Y"
        "JLX)1PB"
        "ZV6)SJG"
        "GWZ)K2L"
        "TWW)J8S"
        "6NN)4VB"
        "PBP)N7J"
        "H7B)XFC"
        "15M)1F2"
        "VH6)BHL"
        "BCC)TFS"
        "3WX)HW8"
        "8LP)24W"
        "LHD)ZDC"
        "XNF)NQP"
        "67M)MJK"
        "WB8)9YS"
        "3SH)6NL"
        "259)BSH"
        "62T)SDM"
        "21P)MCF"
        "NQS)VG3"
        "GTT)NW5"
        "T56)TY5"
        "HXC)36T"
        "L37)MZ6"
        "NYG)C14"
        "J6J)X15"
        "T2S)RY6"
        "KG9)5LR"
        "LJZ)9SM"
        "2XY)K5Q"
        "N7J)MNW"
        "3KZ)6TG"
        "ZYH)RGK"
        "FT1)BW4"
        "MBF)DLQ"
        "RCY)1D8"
        "MQG)ZSY"
        "MNK)69C"
        "7J8)1DV"
        "D9K)9CK"
        "3TN)KKL"
        "GZJ)2B5"
        "GLQ)JYB"
        "JL8)FQ6"
        "3HG)THR"
        "32D)3CY"
        "9FF)SR4"
        "2W5)VFL"
        "XM5)XT4"
        "W66)CBM"
        "5D1)6G5"
        "J9C)HZ6"
        "4N7)6ZT"
        "MKM)2TR"
        "K84)XC7"
        "H59)NYG"
        "9W7)YY4"
        "P6F)PSD"
        "F4T)PWY"
        "RCR)XQX"
        "TNN)4ZX"
        "L8C)HLH"
        "J6G)TRT"
        "ZWS)K1L"
        "XMQ)4B7"
        "Q5S)7NB"
        "Y5W)7QJ"
        "1L3)CT9"
        "KGD)MQG"
        "82P)JVC"
        "CSN)9R3"
        "1DW)G9B"
        "SB9)9X3"
        "9ZC)X3J"
        "DCS)LYH"
        "W7S)XY4"
        "DHZ)JZP"
        "7R1)TKN"
        "STW)BDC"
        "1V6)3K3"
        "FWN)Q79"
        "MDR)DV8"
        "JKN)XGW"
        "4VN)GS5"
        "PWN)JXQ"
        "Z3Y)8VZ"
        "C6M)832"
        "F7N)LNW"
        "XK6)BZD"
        "6N4)TR5"
        "L3P)7ZD"
        "JKZ)8NM"
        "N9Q)PPM"
        "H5Z)C5P"
        "3QX)TS6"
        "PXW)HR9"
        "27Q)CCL"
        "TR3)CRQ"
        "NQ1)Q28"
        "T6X)5NT"
        "5LD)3MW"
        "RYM)62W"
        "B52)D4Q"
        "BQ9)8QF"
        "CF2)5RP"
        "NTC)ZXH"
        "R29)GJF"
        "RLJ)3KZ"
        "WSX)DPR"
        "8FJ)DLN"
        "P9P)RPX"
        "YJC)9MV"
        "4KQ)739"
        "T8Z)137"
        "42B)W6K"
        "9VR)KKM"
        "X66)RCR"
        "KRC)F7N"
        "R32)KRX"
        "62W)KSG"
        "WYY)27H"
        "DLS)CMQ"
        "34V)4KG"
        "H5Z)S7Z"
        "K81)9FM"
        "1JS)41Q"
        "11D)D9K"
        "2WG)7MT"
        "JJZ)2CN"
        "C2G)BPH"
        "PDK)MM8"
        "ZPD)M4N"
        "PL4)N6T"
        "XC7)CZS"
        "T3S)9WJ"
        "HMP)C3M"
        "QCZ)1S3"
        "HYH)T57"
        "FY5)RRW"
        "SSG)V7J"
        "DB1)GS3"
        "RBG)42Y"
        "1HH)MM9"
        "6RW)6B6"
        "KTC)KWZ"
        "FP6)16S"
        "2MB)1V8"
        "N54)S4M"
        "VFL)BWH"
        "5LL)97M"
        "Z6W)HGM"
        "C26)VDW"
        "B3V)VTB"
        "T83)XN4"
        "XWQ)SWT"
        "DC8)XJ3"
        "RW5)8XY"
        "94G)N54"
        "6NY)M4Q"
        "42Y)D6C"
        "J6X)QZD"
        "14K)7LG"
        "SN1)5L3"
        "73N)BM8"
        "D9D)BWC"
        "Z6L)B5F"
        "HCS)43G"
        "XXL)9ZG"
        "ZSJ)HWZ"
        "KFJ)JM8"
        "3PN)QXR"
        "2LK)VYX"
        "X4Y)4BD"
        "W8T)W6R"
        "R59)ZFL"
        "ZVW)23K"
        "WBX)RZ1"
        "44S)BCC"
        "HN4)CZY"
        "KNT)QFD"
        "D5D)KZQ"
        "3FG)4KQ"
        "7KB)FSM"
        "M5Y)BYC"
        "MGX)KVS"
        "9NN)9M5"
        "8MV)6PC"
        "YLH)S3V"
        "6KX)LHD"
        "R6Z)3KQ"
        "HR9)K3K"
        "VCR)BXH"
        "8YP)3QG"
        "3YK)RHN"
        "W2Q)FTH"
        "K48)4ZN"
        "79V)43J"
        "CZS)JRX"
        "Z57)S8J"
        "2QR)6QP"
        "6J1)GSW"
        "ZFJ)L1G"
        "RM3)WV4"
        "MDS)HNX"
        "XTP)D9D"
        "CNZ)Q75"
        "J7H)WBX"
        "88W)9Q7"
        "QFC)HVB"
        "9PD)GTV"
        "MX5)PX8"
        "YDD)3KY"
        "PKY)BDV"
        "NT6)GWZ"
        "CNG)5B9"
        "C9R)RH2"
        "9F6)9LG"
        "Q3K)L3D"
        "797)LKP"
        "Y9D)4YG"
        "Y51)HL4"
        "XX4)L37"
        "BSH)R2H"
        "5HX)5VZ"
        "Y8J)WYB"
        "6VN)MZJ"
        "7JF)2GF"
        "QCF)SH5"
        "WL7)FW9"
        "9G1)KHB"
        "WXW)4WB"
        "KP2)4NT"
        "53M)WLD"
        "84N)NV6"
        "T3L)T3X"
        "J8S)STW"
        "V7J)HNL"
        "GGB)JKZ"
        "85F)6T4"
        "718)7C9"
        "SVS)G23"
        "FM5)C43"
        "61M)PWB"
        "MF6)KSP"
        "PL8)JT2"
        "VDW)F53"
        "QXR)1NH"
        "RTM)9CV"
        "7PV)Z6T"
        "CT9)W51"
        "CM2)CH1"
        "86R)HJ3"
        "84G)G9D"
        "YV7)T8Z"
        "9PT)JWF"
        "JVK)4QP"
        "TKD)5R7"
        "89F)VBZ"
        "SG6)RZN"
        "N7B)RN6"
        "PL3)2YS"
        "D79)DC4"
        "NTX)672"
        "PNJ)MF6"
        "TC6)2D1"
        "TPL)NF3"
        "6Z8)8NX"
        "BPK)WN9"
        "TKK)PDD"
        "56Q)G19"
        "5P2)8SN"
        "GSR)PNK"
        "3WX)2QR"
        "8C7)XCD"
        "97D)L34"
        "B7F)RQ6"
        "9WJ)GWR"
        "2P1)TW8"
        "229)CKC"
        "KWZ)NG5"
        "BDZ)G8Y"
        "FLJ)R26"
        "XTQ)WQ2"
        "S3X)LT5"
        "7C8)M16"
        "9K5)8YJ"
        "VNQ)FBX"
        "RD9)F8T"
        "ZDC)SM9"
        "LYD)V87"
        "6G3)M5L"
        "16Z)9FW"
        "G9D)Z48"
        "6HK)KWH"
        "BHC)TB3"
        "W8Q)913"
        "CRV)DH1"
        "R78)FSZ"
        "H4J)21K"
        "VD9)9T1"
        "2QN)WQ5"
        "LCR)KTM"
        "Z37)MB9"
        "WV4)3LD"
        "137)4S3"
        "KRS)FSP"
        "BWZ)MYH"
        "4BL)19Z"
        "T1T)LN4"
        "X8H)T68"
        "FPX)8C3"
        "3H2)2XB"
        "GS4)3MD"
        "3YJ)12B"
        "SJC)N41"
        "MT9)M5Y"
        "WPB)732"
        "KS4)QJT"
        "QXB)WYF"
        "5VZ)K5Z"
        "MM8)PL8"
        "R6R)FK3"
        "CZ1)6CQ"
        "36K)6YV"
        "TKK)5DK"
        "B7B)RRJ"
        "RBQ)2H2"
        "TK6)WBR"
        "T46)VG9"
        "GD6)QV9"
        "1P9)HXC"
        "NFD)197"
        "YN4)VNQ"
        "1X7)PDT"
        "S8C)99Q"
        "TL5)H59"
        "JPZ)XSN"
        "B5F)CQJ"
        "R5W)PNJ"
        "PDC)LZY"
        "1QJ)FNB"
        "W23)5FX"
        "6T8)HGC"
        "141)V4Q"
        "KVH)MDR"
        "S4B)B1P"
        "B49)37R"
        "PLR)NKJ"
        "9V3)6P3"
        "NTX)PL4"
        "8W2)FRG"
        "2PN)2R1"
        "ZJZ)ZG2"
        "R86)Z43"
        "TNJ)8TC"
        "7YW)S67"
        "ZYH)372"
        "NK6)75K"
        "RWR)KRV"
        "62F)YZP"
        "RK6)LD5"
        "119)FCZ"
        "ZJ3)K5F"
        "KP6)Y51"
        "ST9)8XM"
        "72K)6BF"
    |]

    let orbits = 
        input
            |> Array.map (split ')')

    let map =
        orbits
            |> Array.map (fun x -> x.[0], x.[1])
            |> Array.groupBy(fst)
            |> Array.map (fun (k, v) -> k, v |> Array.map snd)
            |> Map.ofArray

    let calc (map: Map<string, string array>) (planet: string) =
        let rec calc' (checksum: int) (map: Map<string, string array>) (planet: string) =
            match map |> Map.tryFind planet with
            | Some planets ->
                checksum + (planets |> Array.sumBy (calc' (checksum + 1) map))
            | None -> checksum
        calc' 0 map planet

    let sources = orbits
                |> Array.map (fun x -> x.[0])
                |> Set.ofArray

    let effects = orbits
                |> Array.map (fun x -> x.[1])
                |> Set.ofArray

    Set.difference sources effects
        |> Seq.sumBy (calc map)

    // --- Part Two ---

    let rec calc' (move: int) (transfers: Map<string, string array>) (dest: string) (planet: string) =
        match map |> Map.tryFind planet with
        | Some planets when planets |> Array.contains dest -> 
            Some move
        | Some planets ->
            let transfers' = Map.remove planet transfers
            let moves = planets 
                        |> Array.map (calc' (move + 1) transfers' dest)
                        |> Array.filter (Option.isSome)
                        |> Array.map (Option.get)

            if moves.Length = 0 then None
            else Array.min moves |> Some
        | None -> None

    let transfers =
        orbits
            |> Array.collect (fun x -> [|x.[1], x.[0];x.[0], x.[1]|])
            |> Array.groupBy(fst)
            |> Array.map (fun (k, v) -> k, v |> Array.map snd |> Array.filter ((<>)k))
            |> Map.ofArray


    calc' 0 transfers "SAN" "YOU" 

// --- Day 7: Amplification Circuit ---

module Day7=
    open System.Collections.Generic

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

    let execute  (reader: unit -> string) (memory: int array) =
        let rec process' (memory: int array) (output: string) (instructionPointer: int) =
            let instruction = read memory instructionPointer
            let step = offset instruction

            match instruction with
            | Halt -> output
            | Add (i1, i2, dest) -> 
                memory.[dest] <- i1 + i2
                instructionPointer + step |> process' memory output
            | Multiply (i1, i2, dest) -> 
                memory.[dest] <- i1 * i2
                instructionPointer + step |> process' memory output
            | Print i1 -> 
                let output' = sprintf "%s%i" output i1
                instructionPointer + step |> process' memory output'
            | Write i1 -> 
                memory.[i1] <- reader() |> int
                instructionPointer + step |> process' memory output
            | JumpIfTrue (i1, i2) -> 
                if i1 = 0 then
                    instructionPointer + step |> process' memory output
                else
                    i2 |> process' memory output
            | JumpIfFalse (i1, i2) -> 
                if i1 = 0 then
                    i2 |> process' memory output
                else
                    instructionPointer + step |> process' memory output
            | LessThan  (i1, i2, dest) -> 
                memory.[dest] <- if i1 < i2 then 1 else 0
                instructionPointer + step |> process' memory output
            | Equals (i1, i2, dest) -> 
                memory.[dest] <- if i1 = i2 then 1 else 0
                instructionPointer + step |> process' memory output                                         

        process' memory "" 0

    let load (program: string) =
        program
            |> split ','
            |> Array.map int

    let program = "3,8,1001,8,10,8,105,1,0,0,21,34,47,72,93,110,191,272,353,434,99999,3,9,102,3,9,9,1001,9,3,9,4,9,99,3,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,2,9,1002,9,2,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,101,5,9,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,102,4,9,9,1001,9,3,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99"

    let inMemoryReader buffer =
        let queue = new Queue<string>(buffer |> Seq.ofList)
        let read () =
            queue.Dequeue()
        read 

    let amplifiers (program: string) (signal: string) (setting: string) =
        let reader = inMemoryReader [setting;signal]
        program |> load |> execute reader

    seq {
        for i1 in [0..4] do
            for i2 in [0..4] do
                for i3 in [0..4] do
                    for i4 in [0..4] do
                        for i5 in [0..4] do
                            sprintf "%i%i%i%i%i" i1 i2 i3 i4 i5                                    
    } 
        |> Seq.filter (fun str -> str |> Seq.groupBy id |> Seq.length = 5)
        |> Seq.map (Seq.map string >> Seq.fold (amplifiers program) "0")
        |> Seq.maxBy int

    // --- Part Two ---

    let execute' (reader: unit -> string)  (instructionPointer: int) (memory: int array) =
        let rec process' (memory: int array) (instructionPointer: int) =
            let instruction = read memory instructionPointer
            let step = offset instruction

            match instruction with
            | Halt -> None
            | Add (i1, i2, dest) -> 
                memory.[dest] <- i1 + i2
                instructionPointer + step |> process' memory
            | Multiply (i1, i2, dest) -> 
                memory.[dest] <- i1 * i2
                instructionPointer + step |> process' memory
            | Print i1 -> 
                Some (string i1, instructionPointer + step, memory)
            | Write i1 -> 
                memory.[i1] <- reader() |> int
                instructionPointer + step |> process' memory
            | JumpIfTrue (i1, i2) -> 
                if i1 = 0 then
                    instructionPointer + step |> process' memory
                else
                    i2 |> process' memory
            | JumpIfFalse (i1, i2) -> 
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

        process' memory instructionPointer

    let rec feedbackLoop programs signal settings =
        let reader, settings' = 
            match settings with
            | setting::settings ->
                inMemoryReader [setting;signal], settings
            | [] -> inMemoryReader [signal], []   
        
        match programs with
        | (pointer, memory)::programs ->
                match memory |> execute' reader pointer with
                | Some (signal', pointer', memory') -> 
                    feedbackLoop (programs @ [pointer', memory']) signal' settings'
                | None -> 
                    feedbackLoop programs signal settings'
        | [] -> signal 

    let settings =
        seq {
            for i1 in [5..9] do
                for i2 in [5..9] do
                    for i3 in [5..9] do
                        for i4 in [5..9] do
                            for i5 in [5..9] do
                                sprintf "%i%i%i%i%i" i1 i2 i3 i4 i5                                    
        } 
        |> Seq.filter (fun str -> str |> Seq.groupBy id |> Seq.length = 5)

    let amplifiers'() = [0, load  program;0, load  program;0, load  program;0, load  program;0, load  program]
           
    settings
        |> Seq.map (List.ofSeq >> List.map string >> feedbackLoop (amplifiers'()) "0")
        |> Seq.maxBy int

