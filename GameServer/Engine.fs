module Engine

open GameWorld

let createBoard key size =
    sprintf "Board %A size %A initializing" key size |> Log.dbg
    { GameKey = key
      Size = size
      Players = []
      Items = [] }

let spawnPlayer name xy world =
    sprintf "Spawning player %s at %A" name xy |> Log.dbg
    { world with 
        Players = { Id = name
                    Points = 0
                    Position = xy }::world.Players }

let spawnItem itemType xy world =
    sprintf "Spawning %A at %A" itemType xy |> Log.dbg
    { world with
        Items = (xy, itemType)::world.Items }

let applyEvent event world =
    let world = match event with
    | BoardCreated (key, size) -> createBoard key size
    | PlayerSpawned (name, xy) -> spawnPlayer name xy world
    | ItemSpawned (xy, itemType) -> spawnItem itemType xy world
    | Ping chan -> 
        chan.Reply ()
        world
    Reporting.updateState world
    world

let eventProcessor = MailboxProcessor.Start(fun inbox ->
    let rec loop world =
        async { let! event = inbox.Receive()
                return! applyEvent event world 
                        |> loop }
    GameWorld.empty 
    |> loop)



let processEvent = eventProcessor.Post

let ping () =
    eventProcessor.PostAndReply(fun chan -> Ping (chan))