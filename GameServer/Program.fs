open System
open Http

let hr () = 
    printfn "------------------------------------------------------------------"

let prompt (question : string) =
    Console.Write question
    Console.ReadLine().Trim()

let promptInt = prompt >> Int32.Parse

let promptGameKey () =
    prompt "Enter a unique game identifier: "

let promptPlayerNames () =
    let rec loop i acc =
        match prompt <| 
              sprintf "Enter player name #%d (empty when no more): " i 
              with
        | "" -> acc
        | name -> name::acc |> loop (i + 1)
    loop 1 []

let promptBoardDimentions () =
    let width = promptInt "Enter playing board width: "
    let height = promptInt "Enter playing board height: "
    width, height
    
let promptItemCount () =
    promptInt "How many items do you want on the board? "

let startupNew () =
    hr ()
    printfn "SETTING UP A NEW GAME..\n"
    let gameKey = promptGameKey ()
    let playerNames = promptPlayerNames ()
    let size = promptBoardDimentions ()
    let itemCount = promptItemCount ()
    hr ()
    let initEvents = GameWorld.makeGameWorld 
                        playerNames 
                        size 
                        itemCount
    Seq.append 
        [GameWorld.BoardCreated (gameKey, size)]
        initEvents
    |> Seq.iter Engine.processEvent
    Engine.ping () // Syncronize.., wait for all events
    Reporting.queryBoardASCII ()
    |> Log.dbg
    ()

let startupReplay () =
    Log.dbg "REPLAY not implemented!"

[<EntryPoint>]
let main argv = 
    if argv.Length <> 3
    then
        printfn "Usage: gameserver IPaddress port initcommand [gamekey]"
        printfn ""
        printfn "Examples: gameserver localhost 3000 new"
        printfn "Examples: gameserver localhost 3000 replay foobar"
        Environment.Exit 1

    @"
               \.   \.      __,-""-.__      ./   ./
           \.   \`.  \`.-'"""" _,=""=._ """"`-.'/  .'/   ./
            \`.  \_`-''      _,=""=._      ``-'_/  .'/
             \ `-',-._   _.  _,=""=._  ,_   _.-,`-' /
          \. /`,-',-._""""""  \ _,=""=._ /  """"""_.-,`-,'\ ./
           \`-'  /    `-._  ""       ""  _.-'    \  `-'/
           /)   (         \    ,-.    /         )   (\
        ,-'""     `-.       \  /   \  /       .-'     ""`-,
      ,'_._         `-.____/ /  _  \ \____.-'         _._`,
     /,'   `.                \_/ \_/                .'   `,\
    /'       )                  _                  (       `\
            /   _,-'""`-.  ,++|T|||T|++.  .-'""`-,_   \
           / ,-'        \/|`|`|`|'|'|'|\/        `-, \
          /,'             | | | | | | |             `,\
         /'               ` | | | | | '               `\
                            ` | | | '
                              ` | '
       ***  R O B O  B O A R D     G A M E  S E R V E R  ***"
    |> Console.WriteLine

    match argv.[2] with
    | "new" -> startupNew ()
    | "replay" | "load" | "reload" -> startupReplay ()
    | other -> failwith "Unknown initialization argument"

    listenForRequests argv.[0] argv.[1]
    0 // return an integer exit code
