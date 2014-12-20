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
    |> Seq.sortBy (fun p -> p.Points)
    |> Seq.iter (fun p -> 
        appendFormat "PLAYER @{0}: {1}p - {2}\n" [| p.Position ; p.Points ; p.Id |])
    sb.ToString ()
