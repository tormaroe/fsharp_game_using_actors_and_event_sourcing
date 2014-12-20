module Reporting

open GameWorld
open System.Text

let getSym = function
    | Points p -> p.ToString().[0]
    | Transporter -> 'T'

let getBoardArrays world =
    let x, y = world.Size
    let board = Array.init y (fun i -> Array.create x ' ')
    let set (x,y) c = board.[y].[x] <- c
    world.Players |> List.iter (fun p -> set p.Position 'P')
    world.Items |> List.iter (fun (xy,it) -> getSym it |> set xy)
    board

let append (sb:StringBuilder) (s:string) = // usefull?
    ignore <| sb.Append (s)

let getAsciiBoard world =
    let board = getBoardArrays world
    let sb = StringBuilder ()
    ignore <| sb.AppendFormat ("\nGAME: {0} VERSION: {1}\n", world.GameKey, world.Version)
    ignore <| sb.Append("   +")
    ignore <| sb.Append('-', board.[0].Length)
    ignore <| sb.Append("+\n")
    board |> Array.iteri 
                (fun i line ->
                    ignore <| sb.Append(i.ToString().PadLeft(3))
                    ignore <| sb.Append ('|')
                    ignore <| sb.Append (line)
                    ignore <| sb.Append ("|\n"))
    ignore <| sb.Append("   +")
    ignore <| sb.Append('-', board.[0].Length)
    ignore <| sb.Append("+\n")
    world.Players 
    |> Seq.sortBy (fun p -> p.Points)
    |> Seq.iter (fun p -> 
        ignore <| sb.AppendFormat 
            ("PLAYER @{0}: {1}p - {2}\n", p.Position, p.Points, p.Id))
    sb.ToString ()

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