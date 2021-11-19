// https://github.com/rhwy/KataDesign
open System.IO

module Helpers = 
    let split (separator: char) (str: string) = str.Split(separator) |> List.ofSeq
    let writeFile (path: string) (lines: string list) = File.WriteAllLines(path, lines)
    let readFile (path: string) = File.ReadAllText(path)

module PipelineCSV =
        open Helpers

        type CSV = {
            Headers: string list
            Rows: string list
        }

        let private getLines (content: string) = content |> split '\n'
        let private getColumns (line: string) = line |> split ';'

        let read (path: string) = path |> readFile |> getLines

        let filter (lines: string list) = lines |> List.take 2

        let transform (lines: string list) = 
            {
                Headers = lines.Head |> getColumns
                Rows = lines.Tail 
            }

        let write (csv: CSV) (dest: string) = 
            let formatRow (lineNumber: int) (line: string) = 
                line 
                    |> getColumns
                    |> List.zip csv.Headers
                    |> List.fold (fun row (header, col) ->  sprintf "%s %s: %s " row header col) (sprintf "%i) " lineNumber)

            csv.Rows 
                |> List.mapi formatRow
                |> writeFile dest
            
            true

        let write' (dest: string) (csv: CSV) = 
            let formatRow (lineNumber: int) (line: string) = 
                line 
                    |> getColumns
                    |> List.zip csv.Headers
                    |> List.fold (fun row (header, col) ->  sprintf "%s %s: %s " row header col) (sprintf "%i) " lineNumber)

            csv.Rows 
                |> List.mapi formatRow
                |> writeFile dest
            
            true

module Composition=
    module Pipeline =
        type Read<'a> = Read of 'a
        type Filtered<'a> = Filtered of 'a
        type Transformed<'a> = Transformed of 'a
        type Written = bool

        type Path = string

        type Reader<'a> = Path -> 'a
        type Filter<'a> = 'a -> 'a
        type Transformer<'a, 'b> = 'a -> 'b
        type Writer<'a> = 'a -> Path -> bool

        let private readWith (fn: Reader<'a>) = fn >> Read

        let private filterWith (fn: Filter<'a>) = function
            | Read d -> d |> fn |> Filtered

        let private transformWith (fn: Transformer<'a, 'b>) = function 
            | Filtered d -> d |> fn |> Transformed
        
        let private writeWith (fn: Writer<'a>) = function
            | Transformed d -> d |> fn

        let buildPipeline (reader: Reader<'a>) (filter: Filter<'a>) (transformer: Transformer<'a, 'b>) (writer: Writer<'b>) = 
            readWith reader >> filterWith filter >> transformWith transformer >> writeWith writer

    module Program =
        open Pipeline
        open PipelineCSV

        let pipeline = buildPipeline read filter transform write
        
        (__SOURCE_DIRECTORY__ + "/data.csv", __SOURCE_DIRECTORY__ + "/data.txt") ||> pipeline

module Freemonad=
    module Pipeline =
        type PipelineInstruction<'a, 'b, 'c> = 
            | Read of string * ('b -> 'a)
            | Filter of 'b * ('b -> 'a)
            | Transform of 'b * ('c -> 'a)
            | Write of 'c * string * (bool -> 'a)

        let mapI f = function
            | Read (src, next) -> Read (src, next >> f)
            | Filter (data, next) -> Filter (data, next >> f)
            | Transform (data, next) -> Transform (data, next >> f)
            | Write (data, dest, next) -> Write (data, dest, next >> f)

        type Pipeline<'a, 'b, 'c> = 
            | Free of PipelineInstruction<Pipeline<'a, 'b, 'c>, 'b, 'c>
            | Pure of 'a

        let rec bind f = function
            | Free x -> x |> mapI (bind f) |> Free
            | Pure x -> f x

        type PipelineBuilder () =
            member this.Bind (x, f) = bind f x
            member this.Return x = Pure x
            member this.ReturnFrom x = x
            member this.Zero () = Pure ()

        let private write dest data = Free (Write (dest, data, Pure))

        let private filter data = Free (Filter (data, Pure))

        let private transform data = Free (Transform (data, Pure))

        let private read src = Free (Read (src, Pure))

        let buildInterpreter read filter transform write = 
            let rec interpret = function
                | Pure x -> x
                | Free (Read (src, next)) -> src |> read |> next |> interpret
                | Free (Filter  (data, next)) -> data |> filter |> next |> interpret
                | Free (Transform  (data, next)) -> data |> transform |> next |> interpret
                | Free (Write (data, dest, next)) -> data |> write dest |> next |> interpret

            interpret

        let pipeline = PipelineBuilder ()

        let run src dest = 
            pipeline {
                let! data = read src
                let! filteredData = filter data
                let! transform = transform filteredData
                return! write transform dest 
            }

    module Program =
        open Pipeline
        open PipelineCSV

        let interpreter = buildInterpreter read filter transform write'

        run "src" "dest" |> interpreter

module FSM=
    module Pipeline =
        type State<'a, 'b> = 
        | Ready of string
        | Read of 'a
        | Filtered of 'a
        | Transformed of 'b
        | Written of bool

        type Steps<'a, 'b> = 
        | Reader of (string -> 'a)
        | Filter of ('a -> 'a)
        | Transformer of ('a -> 'b)
        | Writer of ('b -> bool)

        let rec apply (state: State<'a, 'b>) (step: Steps<'a, 'b>) = 
            match state, step with
            | Ready path, Reader next -> path |> next |> Read 
            | Read data, Filter next -> data |> next |> Filtered 
            | Filtered data, Transformer next -> data |> next |> Transformed 
            | Transformed data, Writer next -> data |> next |> Written
            | _, _ -> state
    
    module Program =
        open Pipeline
        open PipelineCSV

        let source = __SOURCE_DIRECTORY__ + "/data.csv"
        let dest = __SOURCE_DIRECTORY__ + "/data.txt"

        let pipeline = [
            read |> Reader
            filter |> Filter
            transform |> Transformer
            write' dest |> Writer
        ] 

        (Ready source, pipeline) ||> List.fold apply 