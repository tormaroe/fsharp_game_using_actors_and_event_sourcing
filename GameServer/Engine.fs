module Engine

open GameWorld

let createBoard key size =
    sprintf "Board %A size %A initializing" key size |> Log.dbg
    { GameKey = key
      Version = 0
      Size = size
      Players = []
      Items = [] }

let bumpVersion world = 
    { world with Version = world.Version + 1 }

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

type Message =
    | Event of GameWorld.Event
    | Ping of AsyncReplyChannel<unit>

let applyEvent event world =
    match event with
    | BoardCreated (key, size)   -> bumpVersion <| createBoard key size
    | PlayerSpawned (name, xy)   -> bumpVersion <| spawnPlayer name xy world
    | ItemSpawned (xy, itemType) -> bumpVersion <| spawnItem itemType xy world

let eventProcessor = MailboxProcessor.Start(fun inbox ->
    let rec loop world =
        async { let! message = inbox.Receive()
                match message with
                | Event e -> 
                    let world = applyEvent e world
                    Reporting.updateState world
                    return! loop world
                | Ping chan ->
                    chan.Reply ()
                    return! loop world }
    GameWorld.empty 
    |> loop)

let processEvent gameEvent = 
    eventProcessor.Post <| Event (gameEvent)

let processEvents gameEvents =
    Seq.iter processEvent gameEvents

let ping () =
    eventProcessor.PostAndReply(fun chan -> Ping (chan))