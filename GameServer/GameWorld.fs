﻿module GameWorld

type Coordinates = int * int

type Player = { 
    Id : string
    Points : int
    Position : Coordinates
    Secret : string
    }

type ItemType =
    | Points of int
    | Transporter

type Item = Coordinates * ItemType

type World = {
    GameKey: string
    Version: int
    Size: Coordinates
    Players : Player list
    Items: Item list
    }

let empty = {
    GameKey = ""
    Version = -1
    Size = 0,0
    Players = []
    Items = [] 
    }

type PlayerName = string
type Secret = string

type Event =
    | BoardCreated of string * Coordinates
    | PlayerSpawned of PlayerName * Coordinates * Secret
    | ItemSpawned of Item

type SequencedEvent = int * Event // Usefull?

let rnd = System.Random ()

let rec nUnique n f acc =
    let next = f ()
    match n, Seq.tryFind ((=) next) acc with
    | 0 , _      -> acc
    | _ , None   -> nUnique (n - 1) f <| List.Cons (next, acc)
    | _ , Some _ -> nUnique n f acc

let uniqueRandomPos n (maxX, maxY) =
    nUnique n (fun () -> rnd.Next (0, maxX), rnd.Next (0, maxY)) []

let makeItem position =
    position, 
    (match rnd.Next (0, 100) with
    | n when n < 70 -> rnd.Next (1, 10) |> Points
    | _ -> Transporter)

let (-><-) s x = (Seq.take x s), (Seq.skip x s)

let makeGameWorld namePasswords size itemCount =
    let playerCount = Seq.length namePasswords
    let positions = uniqueRandomPos (itemCount + playerCount) size
    let playerPos, itemPos = positions -><- playerCount
    Seq.append
        (Seq.map2 (fun (n,pw) p -> PlayerSpawned(n, p, pw)) namePasswords playerPos)
        (itemPos |> Seq.map makeItem |> Seq.map ItemSpawned)


