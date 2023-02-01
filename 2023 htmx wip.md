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

let mutable todos = Map<Guid, Todo> []

module Views =
    let private html str = str

    let layout (body: string) =
        html
            $"""
                <html>
                    <head>
                        <script src="https://cdn.tailwindcss.com"></script>
                        <script defer src="https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js"></script>
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
            <li class="flex" id="todo-{todo.Id}">
                <label class="w-full" style="{if todo.Done then "text-decoration:line-through;" else ""}" hx-get="/todos/edit/{todo.Id}" hx-target="#todo-{todo.Id}" hx-swap="outerHTML">
                    {todo.Title}
                </label>
                <input type="checkbox" {if todo.Done then "checked" else ""} hx-patch="/todos/{todo.Id}" hx-target="#todo-{todo.Id}" hx-swap="outerHTML" />
            </li>
        """

    let todoList (todos: Todo list) =
        todos |> List.map todoItem |> String.concat ""

    let index (todos: Todo list) =
        $"""
            <div class="container mx-auto">
                <h1 class="text-2xl mt-4">Todos</h1>
                <form x-data @htmx:after-on-load.camel="$event.target.reset()" hx-target="#todo-list" hx-post="/todos" hx-swap="afterbegin">
                    <div class="flex mt-4">
                        <input class="shadow appearance-none border rounded w-full py-2 px-3 mr-4" type="text" name="title" />
                        <button class="flex-no-shrink p-2 border-2 rounded">Add</button>
                    </div>
                </form>
                <ul id="todo-list">
                    {todos |> todoList}
                </ul>
            </div>
        """

    let editTodo (todo: Todo) =
        html
            $"""
                <form hx-post="/todos/update/{todo.Id}">
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
            let view = todos
                       |> Map.values
                       |> List.ofSeq
                       |> index
                       |> renderHtmlView

            return! view next ctx
        }

    let addTodo next (ctx: HttpContext) =
        task {
            let title =
                ctx.GetFormValue("title")
                    |> Option.defaultValue "A Title"

            let todo =
                { Id = Guid.NewGuid()
                  Title = title
                  Done = false }
                
            todos <- Map.add todo.Id todo todos

            let view = todo
                       |> todoItem
                       |> renderHtmlView

            return! view next ctx
        }

    let editTodo (id: Guid) next (ctx: HttpContext) =
        task {
            let view =
                match Map.tryFind id todos with
                | Some todo ->
                    todo |> editTodo |> renderHtmlView
                | None -> failwith ""

            return! view next ctx
        }

    let updateTodo (id: Guid) next (ctx: HttpContext) =
        task {
            let title = ctx.GetFormValue("title") |> Option.defaultValue "A Title"

            let view =
                match Map.tryFind id todos with
                | Some todo ->
                    let todo' = { todo with Title = title }
                    todos <- Map.add todo.Id todo' todos

                    todo' |> todoItem |> renderHtmlView
                | None -> failwith ""

            return! view next ctx
        }
        
    let toggleTodo (id: Guid) next (ctx: HttpContext) =
        task {
            let view =
                match Map.tryFind id todos with
                | Some todo ->
                    let todo' = { todo with Done = not todo.Done }
                    todos <- Map.add todo.Id todo' todos

                    todo' |> todoItem |> renderHtmlView
                | None -> failwith ""

            return! view next ctx
        }

open Saturn

let routes =
    router {
        get "/" Handlers.allTodos
        post "/todos" Handlers.addTodo
        getf "/todos/edit/%O" Handlers.editTodo
        postf "/todos/update/%O" Handlers.updateTodo
        patchf "/todos/%O" Handlers.toggleTodo
    }

let app = application { use_router routes }

run app

```