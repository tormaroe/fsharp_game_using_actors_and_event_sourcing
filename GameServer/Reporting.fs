module Reporting

let getAsciiBoard world =
    "ASCII"

type Query =
    | UpdateState of GameWorld.World
    | QueryBoardASCII of AsyncReplyChannel<string>

let private reportingActor = MailboxProcessor.Start(fun inbox ->
    let rec loop world =
        async { let! query = inbox.Receive()
                match query with
                | UpdateState world -> return! loop world 
                | QueryBoardASCII chan ->
                    world |> getAsciiBoard |> chan.Reply
                    return! loop world }
    GameWorld.empty
    |> loop)

(* Generic API for sending queries *)

let post = reportingActor.Post
let query = reportingActor.PostAndAsyncReply


(* Query helper API *)

let updateState world = UpdateState(world) |> reportingActor.Post

let queryBoardASCII () = 
    reportingActor.PostAndReply(fun chan -> QueryBoardASCII (chan))