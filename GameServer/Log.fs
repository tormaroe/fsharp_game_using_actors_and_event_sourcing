module Log

let syncedLogger = MailboxProcessor.Start(fun inbox ->
    let rec loop i =
        async { let! msg = inbox.Receive()
                do printfn " %d> %s" i msg
                return! loop (i + 1) }
    loop 1)

let dbg = syncedLogger.Post