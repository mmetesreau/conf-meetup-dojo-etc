``` shell
dotnet new console --language "F#" -f "net6.0" --name htmx -o ./
dotnet add package Saturn --version 0.16.1
```

``` fsharp
module App

open System
open Saturn
open Giraffe

open Giraffe.ViewEngine

let index = 
    html [] [
         head [] [
            script [
                _src "https://unpkg.com/htmx.org@1.8.4"
                _integrity "sha384-wg5Y/JwF7VxGk4zLsJEcAojRtlVp1FKKdGy1qN+OMtdq72WRvX/EdRdqg/LOhYeV"
                _crossorigin "anonymous"
            ] []
        ]
        div [] [
            str "Hello World!"
        ]
    ]

let allRoutes = router {
    get "/api/ping" warbler (json {| pong = DateTime.Now |})
    get "/" (htmlView index)
}

let app = application { use_router allRoutes }

run app
```

``` shell
dotnet run
```

``` fsharp
// ...

let index = 
    html [] [
        div [] [
            div [ attr "hx-get" "/api/ping"] [
                str "Click me"
            ]
        ]
    ]

// ...
```