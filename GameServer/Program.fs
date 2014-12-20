// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Http

[<EntryPoint>]
let main argv = 
    Log.dbg <| sprintf "%A" argv
    listenForRequests argv.[0] argv.[1]
    0 // return an integer exit code
