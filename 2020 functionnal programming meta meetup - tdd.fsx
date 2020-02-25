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

    let withError error x =
        match x with 
        | Some x -> Ok x
        | _ -> Error error    

    module Domain = 
        open System

        type NotEmptyString = NotEmptyString of string   

        let notEmptyStringFromString str = 
            if String.IsNullOrEmpty(str) |> not then
                NotEmptyString str |> Some
            else None

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
                    <!> (notEmptyStringFromString title |> withError "Title can not be empty")
                    <*> (notEmptyStringFromString description |> withError "Description can not be empty"))

    module Db =
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
    
    module Handlers =
        open Domain
        open Db

        let startHandler taskId userId = 
            tryGet taskId 
                >>= tryStart userId 
                >>= tryUpdate taskId

        let closeHandler taskId userId = 
            tryGet taskId 
                >>= tryClose userId 
                >>= tryUpdate taskId

        let createHandler title description = 
            tryCreate title description
                >>= tryInsert

open App.Domain
open App.Handlers

("", "d1") 
    ||> createHandler = Error "Title can not be empty"

("t1", "") 
    ||> createHandler = Error "Description can not be empty"

("t1", "d1") 
    ||> createHandler = 
            Ok (TaskId 1, TodoTask { 
                            Info = { 
                                    Title = NotEmptyString "t1" 
                                    Description = NotEmptyString "d1" } })

(TaskId 1, UserId 1) 
    ||> startHandler = 
            Ok (TaskId 1, InProgressTask { 
                            Info = { 
                                        Title = NotEmptyString "t1"
                                        Description = NotEmptyString "d1" }
                            DevId = UserId 1 })

(TaskId 1, UserId 1) 
    ||> closeHandler  = 
            Ok (TaskId 1, DoneTask { 
                            Info = { 
                                        Title = NotEmptyString "t1"
                                        Description = NotEmptyString "d1" } 
                            DevId = UserId 1
                            QaId = UserId 1 })



    



