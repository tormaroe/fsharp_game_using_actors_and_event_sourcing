module ReportingAscii

open GameWorld
open System.Text

let private getSym = function
    | Points p -> p.ToString().[0]
    | Transporter -> 'T'

let private getBoardArrays world =
    let x, y = world.Size
    let board = Array.init y (fun i -> Array.create x ' ')
    let set (x,y) c = board.[y].[x] <- c
    world.Players |> List.iter (fun p -> set p.Position 'P')
    world.Items |> List.iter (fun (xy,it) -> getSym it |> set xy)
    board
    
let getAsciiBoard world =
    let board = getBoardArrays world
    let sb = StringBuilder ()
    let appendString (s:string) = sb.Append (s) |> ignore
    let appendCharAr (ca:char array) = sb.Append (ca) |> ignore
    let appendRepeat (c:char) n = sb.Append (c, n) |> ignore
    let appendFormat s (args:System.Object array) = sb.AppendFormat (s, args) |> ignore
    appendFormat "\nGAME: {0} VERSION: {1}\n" [| world.GameKey ; world.Version |]
    appendString "   +"
    appendRepeat '-' board.[0].Length
    appendString "+\n"
    board |> Array.iteri 
                (fun i line ->
                    appendString <| i.ToString().PadLeft(3)
                    appendString "|"
                    appendCharAr line
                    appendString "|\n")
    appendString "   +"
    appendRepeat '-' board.[0].Length
    appendString "+\n"
    world.Players 
    |> Seq.sortBy (fun p -> p.Points * -1)
    |> Seq.iter (fun p -> 
        appendFormat "PLAYER @{0}: {1}p - {2}\n" [| p.Position ; p.Points ; p.Id |])
    sb.ToString ()


(*** Tests ***)

module ReportingAsciiTests =

    open NUnit.Framework
    open FsUnit

    [<TestFixture>]
    type ``ASCII reporting tests`` () =
        let world = { GameWorld.empty 
                      with Size = (10, 5)
                           Version = 12
                           GameKey = "FooBar"
                           Players = [{ Id="Player 1"
                                        Points=0
                                        Position=(0,0)
                                        Secret="" }
                                      { Id="Player 2"
                                        Points=3
                                        Position=(1,2)
                                        Secret="" }] 
                           Items = [((0,4), Points(1))
                                    ((9,0), Points(9))
                                    ((1,1), Transporter)] }

        [<Test>] member x.
         ``Should set players and items in a board array og arrays`` () =
            getBoardArrays world
            |> should equal
                [|
                    [|'P';' ';' ';' ';' ';' ';' ';' ';' ';'9'|]
                    [|' ';'T';' ';' ';' ';' ';' ';' ';' ';' '|]
                    [|' ';'P';' ';' ';' ';' ';' ';' ';' ';' '|]
                    [|' ';' ';' ';' ';' ';' ';' ';' ';' ';' '|]
                    [|'1';' ';' ';' ';' ';' ';' ';' ';' ';' '|]
                |]

        [<Test>] member x.
         ``Should make nice ASCII board report`` () =
            getAsciiBoard world
            |> (fun (s:string) -> s.Split [|'\r';'\n'|])
            |> should equal
                [|
                    ""
                    "GAME: FooBar VERSION: 12"
                    "   +----------+"
                    "  0|P        9|"
                    "  1| T        |"
                    "  2| P        |"
                    "  3|          |"
                    "  4|1         |"
                    "   +----------+"
                    "PLAYER @(1, 2): 3p - Player 2" // Note sort order (DESC)
                    "PLAYER @(0, 0): 0p - Player 1"
                    ""
                |]