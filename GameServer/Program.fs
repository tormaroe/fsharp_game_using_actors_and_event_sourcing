open System
open Http

let hr () = 
    printfn "------------------------------------------------------------------"

let setInterval interval f = Async.Start(async {
    let rec loop () =
        Async.Sleep interval |> Async.RunSynchronously
        f ()
        loop ()
    loop () })

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

let genSecret len =
    let chars = [|'A';'C';'D';'E';'F';'H';'J';'K'
                  'L';'M';'N';'P';'Q';'R';'T';'U'
                  'W';'X';'Y';'Z';'2';'3';'4';'7'
                  '9';'%';'d';'!';'r';'e';'g';'i'|]
    let rec loop n secret = 
        if n > 0
        then chars.[GameWorld.rnd.Next(chars.Length)]::secret
             |> loop (n - 1)
        else String.Join("", secret)
    loop len []

let startupNew () =
    hr ()
    printfn "SETTING UP A NEW GAME..\n"
    let gameKey = promptGameKey ()
    let players = promptPlayerNames ()
                  |> Seq.map (fun name -> (name, genSecret 5))
    let size = promptBoardDimentions ()
    let itemCount = promptItemCount ()
    hr ()
    printfn "\nPASSWORDS NEEDED TO ISSUE COMMANDS"
    players |> Seq.iter (fun (n,p) -> printfn "%s: %s" n p)
    hr ()
    printfn "\nPress ENTER to begin...\n"
    Console.ReadLine () |> ignore
    Seq.append 
        [GameWorld.BoardCreated (gameKey, size)]
        (GameWorld.makeGameWorld players size itemCount)
    |> Engine.processEvents
    setInterval 5000 
        (fun () ->
            Reporting.queryBoardASCII ()
            |> Log.dbg)

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
