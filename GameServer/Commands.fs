module Commands

open GameWorld

type Direction =
    | North | South | East | West 
    | NorthEast | SouthEast | NorthWest | SouthWest

type Command =
    | MovePlayer of Secret * Direction

type CommandResult = 
    | CommandSuccess of Event seq
    | CommandFailure of string

let unknownSecret = CommandFailure "Unknown secret"

let rec moveCoordinates (x,y) = function
    | North -> x-1, y | South -> x+1, y
    | East  -> x, y+1 | West  -> x, y-1
    | NorthEast -> Seq.fold moveCoordinates (x,y) [North ; East]
    | SouthEast -> Seq.fold moveCoordinates (x,y) [South ; East]
    | NorthWest -> Seq.fold moveCoordinates (x,y) [North ; West]
    | SouthWest -> Seq.fold moveCoordinates (x,y) [South ; West]

let accept command world =
    match command with
    | MovePlayer (secret, direction) ->
        match getPlayer secret world with
        | None -> unknownSecret
        | Some p ->
            let newXY = moveCoordinates p.Position direction
            // verify legal coordinates
            // make player moved event
            // if xy has item, process and make events
            // elif xy has oponent, process and make events
            CommandSuccess [(PlayerMoved (p.Id, newXY))]


(*** Tests ***)

module CommandsTests =

    open NUnit.Framework
    open FsUnit
    
     [<TestFixture>]
    type ``command tests`` () =
        let world = { GameKey = "TEST"
                      Version = 1
                      Size = 10, 20
                      Players = [{ Id = "P1"
                                   Points = 0
                                   Position = 0, 0
                                   Secret = "password1" }]
                      Items = [] }

        [<Test>] member x.
         ``Should move player`` () =
            world
            |> accept (MovePlayer ("password1", SouthEast))
            |> should equal 
                (CommandSuccess 
                    [(PlayerMoved ("P1", (1, 1)))])
