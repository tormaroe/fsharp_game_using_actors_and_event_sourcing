module Http

open System.Net
open System.Web

let respond (ctx:HttpListenerContext) (code, (result:string)) =
    use res = ctx.Response
    res.StatusCode <- code
    let bytes = ctx.Request.ContentEncoding.GetBytes result
    res.ContentLength64 <- bytes.LongLength
    res.OutputStream.Write(bytes, 0, bytes.Length)

let resourcePath (ctx:HttpListenerContext) =
    HttpUtility.UrlDecode 
        (ctx.Request.Url.AbsolutePath, ctx.Request.ContentEncoding)

let dispatch (ctx:HttpListenerContext) =
    match ctx |> resourcePath with
    | "/"   -> 200, "HOME"
    | _     -> 404, "UNKNOWN RESOURCE"
    |> respond ctx 

let handleRequest (ctx:HttpListenerContext) = 
    async {
        let req = ctx.Request
        sprintf "[%s] %A" req.HttpMethod req.Url
        |> Log.dbg
        dispatch ctx
    }

let listenForRequests address port =
    let listener = new HttpListener()
    sprintf "http://%s:%s/" address port
    |> listener.Prefixes.Add 
    listener.Start()
    while true do
        listener.GetContext()
        |> handleRequest 
        |> Async.Start