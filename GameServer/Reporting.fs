module Reporting

open GameWorld
open ReportingAscii

type Query =
    | UpdateState of GameWorld.World
    | QueryWorld of AsyncReplyChannel<GameWorld.World>
    | QueryBoardASCII of AsyncReplyChannel<string>
    | QueryPlayer of Secret * AsyncReplyChannel<GameWorld.Player option>

let private getPlayer secret world =
    world.Players
    |> List.tryFind (fun p -> p.Secret = secret)
    
let private reportingActor = MailboxProcessor.Start(fun inbox ->
    let rec loop world =
        async { let! query = inbox.Receive()
                match query with
                | UpdateState world -> return! loop world 
                | QueryBoardASCII chan ->
                    world |> getAsciiBoard |> chan.Reply
                    return! loop world 
                | QueryPlayer (secret, chan) ->
                    world |> getPlayer secret |> chan.Reply 
                    return! loop world }
    GameWorld.empty
    |> loop)

(* Generic API for sending queries *)

let post = reportingActor.Post
let query = reportingActor.PostAndAsyncReply

let private querySync qType =
    query (fun chan -> qType (chan))
    |> Async.RunSynchronously

(* Query helper API *)

let updateState world = UpdateState(world) |> post
let queryBoardASCII () = querySync QueryBoardASCII
let queryWorld () = querySync QueryWorld
let queryPlayer secret = 
    query (fun chan -> QueryPlayer (secret, chan))
    |> Async.RunSynchronously

