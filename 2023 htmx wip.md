``` shell
dotnet new console --language "F#" -n htmx -o src
dotnet add src/ package Saturn 
```

``` fsharp
module App

open System
open Saturn
open Giraffe

let routes = router {
    get "/ping" warbler (json {| pong = DateTime.Now |})
}

let app = application { use_router routes }

run app
```

``` shell
dotnet run
```

``` fsharp
module App

open System

type Todo = { Id: Guid; Title: string; Done: bool }

module Infra =
    let mutable private todos = Map<Guid, Todo> []

    let getAll () = todos |> Map.values |> List.ofSeq

    let getById id = Map.tryFind id todos

    let save todo =
        todos <- Map.add todo.Id todo todos
        todo
        
    let delete id =
        todos <- Map.remove id todos

module Views =
    let private html str = str

    let layout (body: string) =
        html
            $"""
                <html>
                    <head>
                        <script src="https://unpkg.com/hyperscript.org@0.9.7"></script>
                        <script src="https://unpkg.com/htmx.org@1.8.5"></script>
                    </head>
                    <body>
                        {body}
                    </body>
                <html>
            """

    let todoItem (todo: Todo) =
        html
            $"""
            <div id="todo-{todo.Id}">
                <label style="{if todo.Done then "text-decoration:line-through;" else ""}" hx-get="/todos/edit/{todo.Id}" hx-target="#todo-{todo.Id}" hx-swap="outerHTML">
                    {todo.Title}
                </label>
                <input type="checkbox" {if todo.Done then "checked" else ""} hx-patch="/todos/{todo.Id}" hx-target="#todo-{todo.Id}" hx-swap="outerHTML" />
                <button hx-delete="/todos/{todo.Id}" _="on htmx:afterOnLoad remove #todo-{todo.Id}">Delete</button>
            </div>
        """

    let todoList (todos: Todo list) =
        todos |> List.map todoItem |> String.concat ""

    let index (todos: Todo list) =
        html
            $"""
                    <h1>Todos</h1>
                    <form hx-target="#todo-list" hx-post="/todos" hx-swap="afterbegin" _="on htmx:afterOnLoad set #titleForm.value to ''" >
                        <div>
                            <input id='titleForm' type="text" name="title" />
                            <button>Add</button>
                        </div>
                    </form>
                    <div id="todo-list">
                        {todos |> todoList}
                    </div>
            """

    let editTodo (todo: Todo) =
        html
            $"""
                <form hx-post="/todos/update/{todo.Id}" hx-swap="outerHTML" _="on htmx:afterOnLoad()>
                    <input type="text" name="title" value="{todo.Title}" />
                    <button>Save</button>
                </form>
            """

module Handlers =
    open Microsoft.AspNetCore.Http
    open Giraffe
    open Views

    let renderHtmlView view = view |> layout |> htmlString

    let allTodos next (ctx: HttpContext) =
        task {
            let view =
                Infra.getAll ()
                |> index
                |> renderHtmlView

            return! view next ctx
        }

    let addTodo next (ctx: HttpContext) =
        task {
            let title = ctx.GetFormValue("title") |> Option.defaultValue "A Title"

            let view =
                Infra.save { Id = Guid.NewGuid(); Title = title; Done = false }
                |> todoItem
                |> renderHtmlView

            return! view next ctx
        }

    let editTodo (id: Guid) next (ctx: HttpContext) =
        task {
            let view =
                match Infra.getById id with
                | None -> failwith ""
                | Some todo ->
                    todo
                    |> editTodo
                    |> renderHtmlView

            return! view next ctx
        }

    let updateTodo (id: Guid) next (ctx: HttpContext) =
        task {
            let title = ctx.GetFormValue("title") |> Option.defaultValue "A Title"

            let view =
                match Infra.getById id with
                | None -> failwith ""
                | Some todo ->
                    Infra.save { todo with Title = title }
                    |> todoItem
                    |> renderHtmlView

            return! view next ctx
        }

    let toggleTodo (id: Guid) next (ctx: HttpContext) =
        task {
            let view =
                match Infra.getById id with
                | None -> failwith ""
                | Some todo ->
                    Infra.save { todo with Done = not todo.Done }
                    |> todoItem
                    |> renderHtmlView
                    
            return! view next ctx
        }
        
    let deleteTodo (id: Guid) next (ctx: HttpContext) =
        task {
            Infra.delete id
           
            return! ( renderHtmlView "") next ctx
        }

open Saturn
open Handlers

let routes =
    router {
        get "/" allTodos
        post "/todos" addTodo
        getf "/todos/edit/%O" editTodo
        postf "/todos/update/%O" updateTodo
        patchf "/todos/%O" toggleTodo
        deletef "/todos/%O" deleteTodo
    }

let app = application { use_router routes }

run app

```