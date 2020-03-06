(* 
En tant qu'utilisateur je veux pouvoir créer une tache 
En tant qu'utilisateur je veux pouvoir commencer une tache existante non commencée
En tant qu'utilisateur je veux pouvoir terminer une tache commencée
*)

module DomainAttempt1 =
    type StatusInfo = Todo | InProgress | Done

    type Task = {
        Title: string
        Description: string
        Status: StatusInfo
        CreatedBy: int
        StartedBy: int
        ClosedBy: int
    }

module DomainAttempt2 = 
    type StatusInfo = 
        | Todo of createdBy: int
        | InProgress of createdBy: int * startedBy: int
        | Done of createdBy: int * startedBy: int * closedBy: int

    type Task = {
        Title: string
        Description: string
        Status: StatusInfo
    }

    let create (title: string) (description: string) (createdBy: int) = 
        { Title = title; Description = description; Status = Todo createdBy }

    let start (task: Task) (startedBy: int) =
        match task.Status with
        | Todo createdBy -> { task with Status = InProgress (createdBy, startedBy)}
        | _ -> failwith "..."

    let close (task: Task) (closedBy: int) =
        match task.Status with
        | InProgress (createdBy, startedBy) -> { task with Status = Done (createdBy, startedBy, closedBy)}
        | _ -> failwith "..."

    // create : string -> string -> int -> Task
    // start  : Task -> int -> Task  
    // close  : Task ->int -> Task

module DomainAttempt3 = 
    type Task = 
        | TodoTask of TodoTask 
        | InProgressTask of InProgressTask 
        | DoneTask of DoneTask
    and TodoTask = {
        Title: string
        Description: string
        CreatedBy: int
    }
    and InProgressTask = {
        TodoTask: TodoTask
        StartedBy: int
    }
    and DoneTask = {
        InProgressTask: InProgressTask
        ClosedBy: int
    }

    let create (title: string) (description: string) (user: int)  = 
        { Title = title; Description = description; CreatedBy = user }

    let start (task: TodoTask) (user: int) =
        { TodoTask = task; StartedBy = user }

    let close (task: InProgressTask) (user: int) =
        { InProgressTask = task; ClosedBy = user }

    // create : string -> string -> int -> TodoTask
    // start  : TodoTask -> int -> InProgressTask
    // close  : InProgressTask -> int -> DoneTask

module DomainAttempt3' =
    type NotEmptyString = NotEmptyString of string   

    type UserId = UserId of int

    type Task = 
        | TodoTask of TodoTask 
        | InProgressTask of InProgressTask 
        | DoneTask of DoneTask
    and TodoTask = {
        Title: NotEmptyString
        Description: NotEmptyString
        CreatedBy: UserId
    }
    and InProgressTask = {
        TodoTask: TodoTask 
        StartedBy: UserId
    }
    and DoneTask = {
        InProgressTask: InProgressTask
        ClosedBy: UserId
    }

    let create (title: NotEmptyString) (description: NotEmptyString) (user: UserId) = 
        { Title = title; Description = description; CreatedBy = user }

    let start (task: TodoTask) (user: UserId) =
        { TodoTask = task; StartedBy = user }

    let close (task: InProgressTask) (user: UserId) =
        { InProgressTask = task; ClosedBy = user }
    
    // create : NotEmptyString -> NotEmptyString -> UserId -> TodoTask
    // start  : TodoTask -> UserId -> InProgressTask
    // close  : InProgressTask -> UserId -> DoneTask

module App =
    module Result =
        let (>>=) a b = Result.bind b a

        let (<!>) f x = Result.map f x

        let (<*>) f x =
            match f, x with
            | Ok f', Ok x' -> Ok (f' x')
            | Error e, Ok x' -> Error e
            | Ok f', Error e -> Error e
            | Error e, Error e' -> Error (sprintf "%s, %s" e e') 

    module Domain = 
        open Result
        open System

        type NotEmptyString = NotEmptyString of string   

        let notEmptyStringFromString error str = 
            if String.IsNullOrEmpty(str) |> not then
                NotEmptyString str |> Ok
            else Error error

        type TaskId = TaskId of int
        type UserId = UserId of int

        type TaskInfo = {
            Title: NotEmptyString
            Description: NotEmptyString
        }

        type Task = 
            | TodoTask of TodoTask 
            | InProgressTask of InProgressTask 
            | DoneTask of DoneTask
        and TodoTask = {
            Title: NotEmptyString
            Description: NotEmptyString
            CreatedBy: UserId
        }
        and InProgressTask = {
            TodoTask: TodoTask 
            StartedBy: UserId
        }
        and DoneTask = {
            InProgressTask: InProgressTask
            ClosedBy: UserId
        }

        let private create (title: NotEmptyString) (description: NotEmptyString) (user: UserId) = 
            { Title = title; Description = description; CreatedBy = user }

        let private start (task: TodoTask) (user: UserId)  =
            { TodoTask = task; StartedBy = user }

        let private close (task: InProgressTask) (user: UserId) =
            { InProgressTask = task; ClosedBy = user }

        // create    : NotEmptyString -> NotEmptyString -> UserId -> TodoTask
        // start     : TodoTask -> UserId -> InProgressTask
        // close     : InProgressTask -> UserId -> DoneTask

        let tryCreate title description user : Result<Task, string> =
            let a = create 
                        <!> (notEmptyStringFromString "Title can not be empty" title)
                        <*> (notEmptyStringFromString  "Description can not be empty" description)
                        <*> Ok user

            TodoTask <!> a

        let tryStart user task : Result<Task, string> = 
            match task with
            | TodoTask task -> 
                user 
                    |> start task 
                    |> InProgressTask 
                    |> Ok
            | _ -> Error (sprintf "%A cannot be started" task)

        let tryClose user task : Result<Task, string> = 
            match task with
            | InProgressTask task -> 
                user
                    |> close task 
                    |> DoneTask 
                    |> Ok
            | _ -> Error (sprintf "%A cannot be closed" task)

        // tryCreate : NotEmptyString -> NotEmptyString -> UserId -> Result<Task, string> 
        // tryStart  : UserId -> TodoTask -> Result<Task, string> 
        // tryClose  : UserId -> InProgressTask -> Result<Task, string>

    module DummyDb =
        open Domain

        let mutable data = Map.empty

        let tryGet (id: TaskId) : Result<Task, string> =
            match data |> Map.tryFind id with
            | Some item -> Ok item
            | _ -> Error (sprintf "%A does not exist" id)

        let tryUpdate (id: TaskId) (task: Task) : Result<TaskId * Task, string> =
            data <- Map.add id task data 

            Ok (id, task)  

        let tryInsert (task: Task) : Result<TaskId * Task, string> =
            let newId = Map.count data + 1 |> TaskId

            data <- Map.add newId task data 

            Ok (newId, task)

        // tryGet     : TaskId -> Result<Task, string> 
        // tryUpdate  : TaskId -> Task -> Result<Task, string> 
        // tryInsert  : Task -> Result<TaskId * Task, string>                 
    
    module Handlers =
        open Domain
        open DummyDb
        open Result

        let createHandler title description userId = 
            userId |> 
                tryCreate title description 
                >>= tryInsert

        let startHandler taskId userId = 
            tryGet taskId 
                >>= tryStart userId 
                >>= tryUpdate taskId

        let closeHandler taskId userId = 
            tryGet taskId 
                >>= tryClose userId 
                >>= tryUpdate taskId

        // tryGet     : string -> string -> UserId -> Result<TaskId * Task, string> 
        // tryUpdate  : TaskId -> UserId -> Result<TaskId * Task, string> 
        // tryInsert  : TaskId -> UserId -> Result<TaskId * TaskId * Task, string>               



open App.Domain
open App.Handlers

createHandler "" "description" (UserId 1) 

createHandler "title" "" (UserId 1)

createHandler "title" "description" (UserId 1)

startHandler (TaskId 1) (UserId 1)

closeHandler (TaskId 1) (UserId 1)