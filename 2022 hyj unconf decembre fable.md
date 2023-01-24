# Fable

### Prérequis

- [Dotnet](https://dotnet.microsoft.com/en-us/download/dotnet/6.0)
- [Nodejs](https://nodejs.org/en/download/)

### Hello Fable

Créer une page index.html :

``` html
<html>
    <head>
    </head>
    <body>
        Hello!
    </body>
</html>
```

Installer un bundler :

``` shell
npm init -y
npm install --save-dev vite
```

Créer un projet dotnet :

``` shell
dotnet new classlib --language "f#" --framework "netstandard2.0" --output src --name App
```

Installer Fable :

``` shell
dotnet new tool-manifest

# compilateur fsharp vers js
dotnet tool install fable --local --version "4.0.0-theta-*"

# gestionnaire de binding Fable
dotnet tool install femto --local
```

Compiler le projet dotnet vers javascript :

``` shell
dotnet fable src/ -o build  
```

Importer le projet compilé :

``` html
<html>
    <head>
        <script type="module" src="./build/Library.js"></script>
    </head>
    <body>
        Hello!
    </body>
</html>
```
 
Lancer le serveur de développement :

``` shell
npx vite
```

Ajouter les scripts dans le package.json :

``` json
"scripts": {
    "start": "dotnet fable watch src -s -o build --run vite"
}
```

### Hello Browser

Installer le binding pour Browser :

``` shell
dotnet femto install Fable.Core src/
dotnet femto install Fable.Browser.Dom src/
```

Modifier Library.fs :

``` fsharp
module App

open Browser

let div = document.createElement("div")

div.textContent <- "Hello browser!"

let container = document.getElementById("container")

container.insertBefore(div, null) |> ignore
```

Modifier index.html :

``` html
<html>
    <head>
        <script type="module" src="./build/Library.js"></script>
    </head>
    <body>
        <div id='container'></div>
    </body>
</html>
```


Lancer la webapp :

``` shell
npm run start
```

### Hello web components

Installer le binding pour Lit :

``` sh
dotnet femto install Fable.Lit src/
```

Modifier Library.fs :

``` fsharp
module App

open Lit

[<LitElement("demo-app")>]
let App() = 
    let _ = LitElement.init(fun cfg -> ())

    html $"""
        <div>Hello web component!</div>
    """
```

Modifier index.html :

``` html
<html>
    <head>
        <script type="module" src="./build/Library.js"></script>
    </head>
    <body>
        <demo-app></demo-app>
    </body>
</html>
```

#### Les caractères spéciaux

| Type             | Exemple                                                 |
|------------------|---------------------------------------------------------|
| Event listeners  | TODO                                                    |
| Attribut booléen | TODO                                                    |
| Propiétés        | TODO                                                    |

#### Les hooks

``` fsharp
module App

open Lit

[<LitElement("demo-app")>]
let App() = 
    let _ = LitElement.init(fun cfg -> ())

    let counter, setCounter = Hook.useState 0

    let inc () = setCounter (counter + 1)
    let dec () = setCounter (counter - 1)

    html $"""
        <div>{counter}</div>
        <button @click={Ev(inc)}>inc</button>
        <button @click={Ev(dec)}>dec</button>
    """
```

#### Les listes

``` fsharp
module App

open Lit

[<LitElement("demo-app")>]
let App() = 
    let _ = LitElement.init(fun cfg -> ())

    let renderItem item =
        html $"""<li>{item}</li>"""

    html $"""
        <ul>
            { [0..10] |> List.map string |> Lit.mapUnique id renderItem }
        </ul>
    """
```

#### Les props

``` fsharp
module App

open Lit

[<LitElement("demo-item")>]
let Item() =
    let _, props = LitElement.init(fun cfg -> 
        cfg.props <- {|
            item = Prop.Of<string option>(None)
        |})

    match props.item.Value with
    | Some item ->
            html $"""<li>{item}</li>"""
    | None -> Lit.nothing

[<LitElement("demo-app")>]
let App() = 
    let _ = LitElement.init(fun cfg -> ())

    let renderItem item =
        html $"""<demo-item .data={item}></demo-item>"""

    html $"""
        <ul>
            { [0..10] |> List.map string |> Lit.mapUnique id renderItem }
        </ul>
    """
```

### Demo

``` fsharp
module App

open System

type Todo = {
    Id: int
    Title: string
    Description: string
    CreatedAt: DateTime
}

open Thoth.Fetch

module Api = 
    let url = "https://[XXX].mockapi.io/todo"

    let fetchAll () = promise {
        return! Fetch.get<_, Todo list>(url, caseStrategy = Thoth.Json.CaseStrategy.CamelCase)
    }

open Lit

[<LitElement("demo-app")>]
let App() = 
    let _ = LitElement.init(fun cfg -> ())

    let todos, setTodos = Hook.useState<Todo list> []

    let refreshTodos () = promise {
        let! todos = Api.fetchAll ()
        setTodos todos
    }

    Hook.useEffectOnce(fun () -> Promise.start(refreshTodos()))

    let renderItem (todo: Todo) = 
        html $"""
            <div>
                <h2>{todo.Title}</h2>
                <p>{todo.Description}</p>
            </div>"""

    html $"""
        <h1>Todos</h1>
        <div>
            { todos |> Lit.mapUnique (fun x -> string (x.Id)) renderItem }
        </div>
    """
```

### Sources

- [Fable](https://fable.io/)
- [Lit](https://lit.dev/)
- [Fable Lit](https://fable.io/Fable.Lit/)
