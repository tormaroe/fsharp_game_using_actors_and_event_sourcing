module Http

open System.Net
open System.Web
open SimpleJSON

let respond (ctx:HttpListenerContext) (code, (result:string)) =
    Log.dbg <| sprintf "<< %d" code
    use res = ctx.Response
    res.StatusCode <- code
    let bytes = ctx.Request.ContentEncoding.GetBytes result
    res.ContentLength64 <- bytes.LongLength
    res.OutputStream.Write(bytes, 0, bytes.Length)

let handleGetPlayer (ctx:HttpListenerContext) =
    let secret = ctx.Request.Headers.Get("password")
    match Reporting.queryPlayer secret with
    | Some player -> 200, toJson player
    | None -> 401, "Unknown secret, can't get player info"

let resourcePath (ctx:HttpListenerContext) =
    HttpUtility.UrlDecode 
        (ctx.Request.Url.AbsolutePath, ctx.Request.ContentEncoding)

let dispatch (ctx:HttpListenerContext) =
    match ctx |> resourcePath with
    | "/"       -> 200, "HOME"
    | "/player" -> handleGetPlayer ctx
    | _         -> 404, "UNKNOWN RESOURCE"
    |> respond ctx 

let handleRequest (ctx:HttpListenerContext) = 
    async { let req = ctx.Request
            sprintf ">> [%s] %A" req.HttpMethod req.Url
            |> Log.dbg
            dispatch ctx }

let listenForRequests address port =
    let listener = new HttpListener()
    sprintf "http://%s:%s/" address port
    |> listener.Prefixes.Add 
    listener.Start()
    while true do
        listener.GetContext()
        |> handleRequest 
        |> Async.Start