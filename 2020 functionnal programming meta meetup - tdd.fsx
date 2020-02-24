module DomainAttempt1 =
    type StatusInfo = Todo | InProgress | Done

    type Task = {
        Title: string
        Description: string
        Status: StatusInfo
        DevId: int
        QaId: int
    }

module DomainAttempt2 = 
    type StatusInfo = 
        | Todo 
        | InProgress of devId: int
        | Done of devId: int * qaId: int

    type Task = {
        Title: string
        Description: string
        Status: StatusInfo
    }

    let start (devId: int) (task: Task) =
        match task.Status with
        | Todo -> { task with Status = InProgress devId}
        | _ -> failwith "..."

    let close (qaId: int) (task: Task) =
        match task.Status with
        | InProgress devId -> { task with Status = Done (devId, qaId)}
        | _ -> failwith "..."

module DomainAttempt3 = 
    type TaskInfo = {
        Title: string
        Description: string
    }

    type Task = 
        | TodoTask of TodoTask 
        | InProgressTask of InProgressTask 
        | DoneTask of DoneTask
    and TodoTask = {
        Info: TaskInfo 
    }
    and InProgressTask = {
        Info: TaskInfo 
        DevId: int
    }
    and DoneTask = {
        Info: TaskInfo
        DevId: int
        QaId: int
    }

    let start (devId: int) (task: TodoTask) =
        { Info = task.Info; DevId = devId }

    let close (qaId: int) (task: InProgressTask) =
        { Info = task.Info; DevId = task.DevId ;QaId = qaId }

module DomainAttempt3' =
    type NotEmptyString = NotEmptyString of string   

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
        Info: TaskInfo 
    }
    and InProgressTask = {
        Info: TaskInfo 
        DevId: UserId
    }
    and DoneTask = {
        Info: TaskInfo
        DevId: UserId
        QaId: UserId
    }

    let start (devId: UserId) (task: TodoTask) =
        { Info = task.Info; DevId = devId }

    let close (qaId: UserId) (task: InProgressTask) =
        { Info = task.Info; DevId = task.DevId ;QaId = qaId }

module App =
    let (>>=) a b = Result.bind b a

    let (<!>) f x = Result.map f x

    let (<*>) f x =
        match f, x with
        | Ok f', Ok x' -> Ok (f' x')
        | Error e, Ok x' -> Error e
        | Ok f', Error e -> Error e
        | Error e, Error e' -> Error (sprintf "%s, %s" e e') 

    module Domain = 
        open System

        type NotEmptyString = NotEmptyString of string   

        let notEmptyStringFromString str error = 
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
            Info: TaskInfo 
        }
        and InProgressTask = {
            Info: TaskInfo 
            DevId: UserId
        }
        and DoneTask = {
            Info: TaskInfo
            DevId: UserId
            QaId: UserId
        }

        let private start (devId: UserId) (task: TodoTask) =
            { Info = task.Info; DevId = devId }

        let private close (qaId: UserId) (task: InProgressTask) =
            { Info = task.Info; DevId = task.DevId ;QaId = qaId }

        let private create title description = 
            { Info = { Title = title; Description = description} }

        let tryStart userId task : Result<Task, string> = 
            match task with
            | TodoTask task -> 
                start userId task |> InProgressTask |> Ok
            | _ -> Error (sprintf "%A cannot be started" task)

        let tryClose userId task : Result<Task, string> = 
            match task with
            | InProgressTask task -> 
                 close userId task |> DoneTask |> Ok
            | _ -> Error (sprintf "%A cannot be closed" task)

        let tryCreate (title: string) (description: string)  : Result<Task, string> =
            TodoTask <!> 
                (create 
                    <!> notEmptyStringFromString title "Title can not be empty"
                    <*> notEmptyStringFromString description "Description can not be empty")

    module Db =
        open Domain

        let mutable data = Map.empty

        let newTaskId () = data.Count + 1 |> TaskId

        let tryGet (id: TaskId) : Result<Task, string> =
            match data |> Map.tryFind id with
            | Some item -> Ok item
            | _ -> Error (sprintf "%A does not exist" id)

        let trySave (id: TaskId) (task: Task) : Result<TaskId * Task, string> =
            data <- Map.add id task data 

            Ok (id, task)  
    
    module Handlers =
        open Domain
        open Db

        let startHandler taskId userId = 
            tryGet taskId 
                >>= tryStart userId 
                >>= trySave taskId

        let closeHandler taskId userId = 
            tryGet taskId 
                >>= tryClose userId 
                >>= trySave taskId

        let createHandler title description = 
            tryCreate title description
                >>= trySave (newTaskId())

open App.Domain
open App.Handlers

("", "d1") ||> createHandler 

("t1", "") ||> createHandler 

("t1", "d1") ||> createHandler 

(TaskId 1, UserId 1) ||> startHandler 

(TaskId 2, UserId 1) ||> closeHandler 



    



