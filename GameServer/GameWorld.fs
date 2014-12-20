module GameWorld

type Coordinates = int * int

type Player = { 
    Id : string
    Points : int
    Position : Coordinates
}

type ItemType =
    | Points of int
    | Transporter

type Item = Coordinates * ItemType

type World = {
    Size: Coordinates
    Players : Player seq
    Items: Item seq 
}

type Event =
    | PlayerSpawned of string * Coordinates
    | ItemSpawned of ItemType * Coordinates

type SequencedEvent = int * Event // Usefull?

let rnd = System.Random ()

let makeItem position =
    (match rnd.Next (0, 100) with
    | n when n < 70 -> rnd.Next (1, 10) |> Points
    | _ -> Transporter), position

let rec uniqueRandomPos n (maxX, maxY) acc =
    let pos = rnd.Next (0, maxX), rnd.Next (0, maxY)
    match n, Seq.tryFind ((=) pos) acc with
    | 0 , _ -> acc
    | _ , None -> List.Cons (pos, acc) 
                  |> uniqueRandomPos (n - 1) (maxX, maxY)
    | _ , Some _ -> uniqueRandomPos n (maxX, maxY) acc

let split x s =
    (Seq.take x s), (Seq.skip x s)

let makeGameWorld names size itemCount =
    let playerCount = Seq.length names
    let positions = uniqueRandomPos (itemCount + playerCount) size []
    let playerPos, itemPos = positions |> split playerCount
    Seq.append
        (Seq.map2 (fun n p -> PlayerSpawned(n, p)) names playerPos)
        (itemPos |> Seq.map makeItem |> Seq.map ItemSpawned)


