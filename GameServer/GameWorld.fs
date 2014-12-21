module GameWorld

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


(*** Tests ***)

module GameWorldTests =

    open NUnit.Framework
    open FsUnit

    [<Test>]
    let ``Should get x unique results from f`` () =
        let values = ref [1;1;2;3;4;3;2;5;6;6;7]
        let f () =
            let res = List.head !values
            values := List.tail !values
            res
        nUnique 5 f [] |> should equal [5;4;3;2;1]

    [<Test>]
    let ``Should split sequence after x elements`` () =
        let seq1, seq2 = [10;20;30;40;50;60] -><- 4
        seq1 |> List.ofSeq |> should equal [10;20;30;40]
        seq2 |> List.ofSeq |> should equal [50;60]

    [<Test>]
    let ``Should make game with correct events`` () =
        makeGameWorld
            [("Player1", "PW1");("Player2", "PW2")]
            (10,12)
            10
        |> Seq.countBy (fun e ->
                            match e with
                            | PlayerSpawned (_,_,_) -> "PlayerSpawned"
                            | ItemSpawned (_,_) -> "ItemSpawned")
        |> should equal [("PlayerSpawned", 2)
                         ("ItemSpawned", 10)]