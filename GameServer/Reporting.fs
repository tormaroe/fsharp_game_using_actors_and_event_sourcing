module Reporting

open GameWorld
open ReportingAscii

type Query =
    | UpdateState of GameWorld.World
    | QueryWorld of AsyncReplyChannel<GameWorld.World>
    | QueryBoardASCII of AsyncReplyChannel<string>
    | QueryPlayer of Secret * AsyncReplyChannel<GameWorld.Player option>
    
// TODO: If an error is thrown in processor thread
//       nothing good happens. Example: queryBoardASCII of GameWorld.empty
//       Need to trap exceptions, report and continue

let private reportingActor = MailboxProcessor.Start(fun inbox ->
    let rec loop world =
        async { let! query = inbox.Receive()
                match query with
                | UpdateState world -> return! loop world 
                | QueryBoardASCII chan ->
                    world |> getAsciiBoard |> chan.Reply
                    return! loop world 
                | QueryWorld (chan) ->
                    world |> chan.Reply 
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


(*** Tests ***)

module ReportingTests =

    open NUnit.Framework
    open FsUnit

    [<Test>]
    let ``Should get player from updated board`` () =
        { GameWorld.empty
            with Players = [{ Id="Player 1"
                              Points=1
                              Position=(1,1)
                              Secret="pass1" }
                            { Id="Player 2"
                              Points=2
                              Position=(2,2)
                              Secret="pass2" }] }
        |> updateState
        "bad_password" |> queryPlayer |> should equal None
        "pass2" |> queryPlayer |> should equal (Some { Id="Player 2"
                                                       Points=2
                                                       Position=(2,2)
                                                       Secret="pass2" })

    [<Test>]
    let ``Should get ASCII board via reporting actor`` () =
        updateState 
            { GameWorld.empty with GameKey="FooBar"; Size=(2,2) }
        queryBoardASCII () |> should contain "GAME: FooBar"
             