// https://github.com/arolla/the-castle
type Jauge = int

type Cité = {
    Or : Jauge
    Blé : Jauge
    Santé : Jauge
    Population: Jauge
    Armée : Jauge
} 

type Face = Récolte | Commerce | Peste | Grele | Medicament

let ``jauge maximale`` = 10
let ``jauge minimale`` = 0

let ``valider la jauge`` (jauge : Jauge) =
    match jauge with
    | jauge when jauge > ``jauge maximale`` -> ``jauge maximale``
    | jauge when jauge < ``jauge minimale`` -> ``jauge minimale``
    | _ -> jauge 

let ``enlever un a la jauge`` (jauge : Jauge) = jauge - 1 |> ``valider la jauge`` 
let ``enlever deux a la jauge`` (jauge : Jauge) = jauge - 2 |> ``valider la jauge`` 
let ``ajouter un a la jauge`` (jauge : Jauge) = jauge + 1 |> ``valider la jauge`` 

let ``mettre a jour la jauge`` (face : Face) (cité : Cité) = 
    match face with 
    | Récolte -> { cité with Blé = ``ajouter un a la jauge`` cité.Blé }
    | Commerce -> { cité with Or = ``ajouter un a la jauge``  cité.Or }
    | Medicament -> { cité with Santé = ``ajouter un a la jauge`` cité.Santé }
    | Peste -> { cité with Santé = ``enlever un a la jauge`` cité.Santé }
    | Grele -> { cité with Blé = ``enlever un a la jauge`` cité.Blé }

let ``influer les jauges`` (cité : Cité) = 
    let ``influer la jauge santé`` (cité : Cité) = 
        match cité with 
        | cité when cité.Santé > 8 -> { cité with Population = ``ajouter un a la jauge`` cité.Population }
        | cité when cité.Santé < 2 -> { cité with Population = ``enlever deux a la jauge`` cité.Population }
        | cité when cité.Santé < 4 -> { cité with Population = ``enlever un a la jauge`` cité.Population }
        | _ -> cité

    let ``influer la jauge blé`` (cité : Cité) = 
        match cité with 
        | cité when cité.Blé >= 8 -> { cité with Population = ``ajouter un a la jauge`` cité.Santé }
        | cité when cité.Blé < 1 -> { cité with Population = ``enlever deux a la jauge`` cité.Santé }
        | cité when cité.Blé < 3 -> { cité with Population = ``enlever un a la jauge`` cité.Santé }
        | _ -> cité

    cité 
        |> ``influer la jauge santé`` 
        |> ``influer la jauge blé``

let ``lancer le dés`` = 
    let dés() = 
        let random = System.Random()
        fun () ->
            match random.Next(1,6) with 
            | 1 -> Récolte
            | 2 -> Commerce
            | 4 -> Grele
            | 5 -> Medicament
            | 3 | _ -> Peste
    dés()

let ``tour de jeu`` (cité : Cité) = 
    let face = ``lancer le dés``()
    cité 
    |> ``influer les jauges``
    |> ``mettre a jour la jauge`` face

let ``est tombé`` (cité : Cité) = 
    cité.Population = ``jauge minimale`` 
    || cité.Population = ``jauge maximale`` 
    || cité.Armée = ``jauge maximale``

let rec jouer (cité : Cité) =
    printfn "%A" cité
    if  cité |> ``est tombé`` then cité
    else jouer <| ``tour de jeu`` cité  

{
    Or = 5
    Blé = 5
    Santé = 5
    Population = 5
    Armée = 5
} |> jouer 
