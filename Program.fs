open System.Security.Cryptography
open System

let random = RandomNumberGenerator.Create()

let getOneRandomInt (min: int) (max: int) =
    let range = max - min + 1
    let bytes = Array.zeroCreate 4
    random.GetBytes(bytes)
    let num = BitConverter.ToInt32(bytes, 0)
    min + abs(num) % range

type Suit = | Trump | Wand | Sword | Cup | Coin

type Card = Card of Suit * int

let drawRandomCard = 
    let randomSuit = getOneRandomInt 0 4
    let randomOrdinal = getOneRandomInt 0 13
    let suit = 
        match randomSuit with
        | 0 -> Trump | 1 -> Wand | 2 -> Sword | 3 -> Cup | 4 -> Coin
        | _ -> raise (invalidOp "RNG machine broke")
    let ordinal = 
        match suit with
        | Trump -> randomOrdinal % 22
        | _ -> randomOrdinal % 14
    Card (suit, ordinal)

let getTrumpName =
    function
    | 0 -> "The Fool" | 1 -> "The Magician" | 2 -> "The High Priestess" | 3 -> "The Empress"
    | 4 -> "The Emperor" | 5 -> "The Hierophant" | 6 -> "The Lovers" | 7 -> "The Chariot"
    | 8 -> "Strength" | 9 -> "The Hermit" | 10 -> "Wheel of Fortune" | 11 -> "Justice"
    | 12 -> "The Hanged Man" | 13 -> "Death" | 14 -> "Temperance" | 15 -> "The Devil"
    | 16 -> "The Tower" | 17 -> "The Star" | 18 -> "The Moon" | 19 -> "The Sun"
    | 20 -> "Judgement" | 21 -> "Za Warudo"
    | _ -> raise (invalidOp "RNG machine broke")

let getMinorName card = 
    match card + 1 with
    | 14 -> "King"
    | 13 -> "Queen"
    | 12 -> "Knight"
    | 11 -> "Page"
    | _ -> (card + 1).ToString()

let printRandomCard (Card (suit, card)) =
    match suit with
    | Trump -> printf "%s" (getTrumpName card)
    | Wand  -> printf "%s of Wands" (getMinorName card)
    | Sword -> printf "%s of Swords" (getMinorName card)
    | Cup   -> printf "%s of Cups" (getMinorName card)
    | Coin  -> printf "%s of Pentacles" (getMinorName card)

let matchSuit (input:string) =
    match input.ToLower() with
    | "major" | "m"
    | "trump" | "t" -> Trump
    | "wand"  | "w" -> Wand
    | "sword" | "s" -> Sword
    | "cup"   | "c" -> Cup
    | "coin"  | "p" -> Coin
    | _ -> raise (invalidOp "Bad suit!")

let tryParseSuit (suit:string) =
    try
        Some (matchSuit suit)
    with
        | :? InvalidOperationException -> printfn "Tried to parse bad suit!"; None

let rec readInput () =
    let randomDraw = drawRandomCard
    Console.Write("Enter a suit guess: ")
    let input = Console.ReadLine().Trim()
    if input.ToLower() = "exit"
        then ()
    else
        match tryParseSuit(input) with
        | Some(e) -> 
            // Print out actual draw
            printRandomCard randomDraw
            Console.WriteLine()
            readInput()
        | None -> readInput()

[<EntryPoint>]
let main _ =
    readInput()
    0 // return an integer exit code
