open System

module Step1 = 

    // b = f(a)
    // c = g(b)

    let parse (entry: string) : int =
        Int32.Parse(entry)

    let inc (value: int) = 
        value + 1

    let value = parse "1"
    let newValue = inc value
    
    // c = f(g(a))

    let newValue' = inc ( parse ("1") )

    // c(a) = (g ∘ f)(a)

    let process = parse >> inc

    let newValue'' = process "1"

module Step2 =

    // f: string -> int 
    //
    // Vraiment ?

    let parse (entry: string) : int =
        Int32.Parse(entry)
    
    parse "oups"

    // En fait on a plutot quelque chose comme
    //
    // f: string -> int 
    // f: exception 
    //
    // Tentons donc de modéliser ce "peut etre"
    //
    // f: string -> int  |
    //                   | -> f: string -> Maybe int
    // f: exception      |

    type PeutEtre = 
        | UneValeur of int
        | Rien

    let tryParse (entry: string) : PeutEtre =
        try 
            Int32.Parse(entry) |> UneValeur
        with
        | _ -> Rien

    tryParse "oups"

    // En fait g ∘ f n'était pas string -> int mais   
    // 
    // g ∘ f: string -> int
    // g ∘ f : exception   
    //
    // Tentons de modéliser ce "peut etre"
    // g ∘ f: string -> int  |
    //                       | -> g ∘ f: string -> Maybe int
    // g ∘ f: exception      |   

    let tryInc (value: PeutEtre) : PeutEtre = 
        match value with
        | UneValeur value -> value + 1 |> UneValeur
        | Rien -> Rien

    // Ca marche :)    

    let tryProcess = tryParse >> tryInc

    tryProcess "1"
    tryProcess "oups"

    // AKA Option<'a>

module Step3 =

    // Refactorisons un peu

    let tryParse (entry: string) : int option =
        try 
            Int32.Parse(entry) |> Some
        with
        | _ -> None

    module RefactorStep1 = 

        // Isolons la logique métier

        let tryInc (value: int option) : int option = 
            let inc (value: int) = value + 1 |> Some

            match value with
            | Some value -> inc value
            | None -> None

        // Ca marche toujours :)    

        let tryProcess = tryParse >> tryInc

        tryProcess "1"
        tryProcess "oups"

     module RefactorStep2 = 

        // Puis découplons la logique métier en l'injectant

        let tryTo inc value  = 
            match value with
            | Some value -> inc value
            | None -> None

        let inc (value: int) = value + 1 |> Some
        
        // Ca marche toujours :)    

        let tryProcess = tryParse >> tryTo inc

        // Et en fait

        let ``try g rond f`` f g a = tryTo g (f a)
        
        let (>=>) = ``try g rond f``

        let tryProcess' = tryParse >=> inc

        tryProcess "1"
        tryProcess "oups"

        // AKA Fish operator

module Step4 = 

    // En fait ici on a une exception et on voudrait plutot modéliser une failure pour ne pas perdre l'information
    // f: string -> int  |
    //                   | -> f: string -> Maybe int or exception
    // f: exception      |

    type PeutEtre = 
    | Ok of int
    | Exception of exn

    let tryParse (entry: string) : PeutEtre =
        try 
            Int32.Parse(entry) |> Ok
        with
        | ex -> Exception ex

    tryParse "oups"

    let inc (value: int) = value + 1 |> Ok

    let (>=>) f g arg = 
        match f arg with
        | Ok value -> g value
        | Exception ex -> Exception ex

    let tryProcess = tryParse >=> inc

    tryProcess "1"
    tryProcess "oups"

    // AKA Result<'a, 't>

module Step5 =
    let _ = ()