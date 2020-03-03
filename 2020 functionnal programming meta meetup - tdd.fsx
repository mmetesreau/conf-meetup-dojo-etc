module DomainAttempt1 =
    type StatusInfo = Todo | InProgress | Done

    type Task = {
        Title: string
        Description: string
        Status: StatusInfo
        AssignedTo: int
        ClosedBy: int
    }
        
module DomainAttempt2 = 
    type StatusInfo = 
        | Todo 
        | InProgress of assignedTo: int
        | Done of assignedTo: int * closedBy: int

    type Task = {
        Title: string
        Description: string
        Status: StatusInfo
    }

    let start (assignedTo: int) (task: Task) =
        match task.Status with
        | Todo -> { task with Status = InProgress assignedTo}
        | _ -> failwith "..."

    let close (closedBy: int) (task: Task) =
        match task.Status with
        | InProgress assignedTo -> { task with Status = Done (assignedTo, closedBy)}
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
        AssignedTo: int
    }
    and DoneTask = {
        Info: TaskInfo
        AssignedTo: int
        ClosedBy: int
    }

    let start (assignedTo: int) (task: TodoTask) =
        { Info = task.Info; AssignedTo = assignedTo }

    let close (closedBy: int) (task: InProgressTask) =
        { Info = task.Info; AssignedTo = task.AssignedTo; ClosedBy = closedBy }

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
        AssignedTo: UserId
    }
    and DoneTask = {
        Info: TaskInfo
        AssignedTo: UserId
        ClosedBy: UserId
    }

    let start (assignedTo: UserId) (task: TodoTask) =
        { Info = task.Info; AssignedTo = assignedTo }

    let close (closedBy: UserId) (task: InProgressTask) =
        { Info = task.Info; AssignedTo = task.AssignedTo; ClosedBy = closedBy }

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
            Info: TaskInfo 
        }
        and InProgressTask = {
            Info: TaskInfo 
            AssignedTo: UserId
        }
        and DoneTask = {
            Info: TaskInfo
            AssignedTo: UserId
            ClosedBy: UserId
        }

        let private start (assignedTo: UserId) (task: TodoTask) =
            { Info = task.Info; AssignedTo = assignedTo }

        let private close (closedBy: UserId) (task: InProgressTask) =
            { Info = task.Info; AssignedTo = task.AssignedTo; ClosedBy = closedBy }

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

        let tryCreate (title: string) (description: string) : Result<Task, string> =
            TodoTask <!> 
                (create 
                    <!> (notEmptyStringFromString "Title can not be empty" title)
                    <*> (notEmptyStringFromString  "Description can not be empty" description))

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
    
    module Handlers =
        open Domain
        open DummyDb
        open Result

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

    module Tests =
        let shouldBe (expected: 'a) (actual: 'a)   =
            if actual = expected then ()
            else 
                failwith (sprintf "\n expected \n %A \n but got \n %A" expected actual)

        let shouldBeError (expected: 'b) (actual: Result<'a, 'b>) =
            let expectedError : Result<'a, 'b> = Error expected

            actual 
                |> shouldBe expectedError

        let shouldBeOk (expected: 'a) (actual: Result<'a, 'b>) =
            let expectedOk : Result<'a, 'b> = Ok expected

            actual 
                |> shouldBe expectedOk

open App.Domain
open App.Handlers
open App.Tests

("", "d1")
    ||> createHandler  
    |> shouldBeError "Title can not be empty"

("", "d1") 
    ||> createHandler
    |> shouldBeError "Title can not be empty"

("t1", "") 
    ||> createHandler
    |> shouldBeError "Description can not be empty"

("t1", "d1") 
    ||> createHandler
    |> shouldBeOk (TaskId 1, TodoTask { Info = { Title = NotEmptyString "t1"; Description = NotEmptyString "d1" } })

(TaskId 1, UserId 1) 
    ||> startHandler
    |> shouldBeOk (TaskId 1, InProgressTask { Info = { Title = NotEmptyString "t1"; Description = NotEmptyString "d1" }; AssignedTo = UserId 1 })

(TaskId 1, UserId 1) 
    ||> closeHandler
    |> shouldBeOk (TaskId 1, DoneTask { Info = { Title = NotEmptyString "t1"; Description = NotEmptyString "d1" }; AssignedTo = UserId 1; ClosedBy = UserId 1 })



    



