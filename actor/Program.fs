open System
open Suave
open Suave.Filters
open Suave.Successful
open Suave.RequestErrors
open Suave.Operators


type Message =
    | Add of int
    | Fetch of AsyncReplyChannel<int>

let log msg =
    let now = DateTime.Now.ToShortTimeString()
    printfn "%s %A" now msg

let act = MailboxProcessor<Message>.Start(fun inbox -> 
    let rec loop (state) = async {
        do log state
        let! msg = inbox.Receive()
        do log msg

        match msg with
        | Add a -> 
            return! loop (state + a)
        | Fetch r -> 
            r.Reply(state)
            return! loop state }
    loop 0)

let add (a) =
  context (fun ctx ->
    act.Post (Add a)
    OK(sprintf "%d posted<br> For check state send get-request to /fetch" a))
      
let app : WebPart =
  choose 
    [   
        path  "/fetch/" >=>  request (fun ctx ->
            let data = act.PostAndReply(fun r -> Fetch r)
            OK(sprintf "Current State = %d.<br> For add  data send get-request  by example: /add/1" data))
        pathScan "/add/%d" add
        NOT_FOUND "Found no handlers" ]

[<EntryPoint>]
let main argv =
   
    Async.Sleep 1000 |> Async.RunSynchronously
    startWebServer defaultConfig app 
    exit 0
