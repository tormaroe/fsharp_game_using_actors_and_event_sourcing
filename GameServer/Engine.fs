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

let spawnPlayer (name, xy, secret) world =
    sprintf "Spawning player %s at %A" name xy |> Log.dbg
    { world with 
        Players = { Id = name
                    Points = 0
                    Position = xy
                    Secret = secret }::world.Players }

let spawnItem itemType xy world =
    sprintf "Spawning %A at %A" itemType xy |> Log.dbg
    { world with
        Items = (xy, itemType)::world.Items }

let applyEvent event world =
    match event with
    | BoardCreated (key, size)   -> bumpVersion <| createBoard key size
    | PlayerSpawned (a,b,c)      -> bumpVersion <| spawnPlayer (a,b,c) world
    | ItemSpawned (xy, itemType) -> bumpVersion <| spawnItem itemType xy world

let eventProcessor = MailboxProcessor.Start(fun inbox ->
    let rec loop world =
        async { let! event = inbox.Receive()
                let world = applyEvent event world
                Reporting.updateState world
                return! loop world }
    GameWorld.empty 
    |> loop)

let processEvent = eventProcessor.Post

let processEvents gameEvents =
    Seq.iter processEvent gameEvents