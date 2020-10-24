open System.Security.Cryptography
open System

let random = RandomNumberGenerator.Create()

let getOneRandomInt =
    let bytes = [| 0uy;|]
    // Get 1 random byte
    random.GetBytes(bytes, 0, 1)
    // Cast to int
    int bytes.[0]

type Suit = | Trump | Wand | Sword | Cup | Coin

type Card = Card of Suit * int

let drawRandomCard = 
    // Take modulus to find suit
    let randomSuit = getOneRandomInt
    let randomOrdinal = getOneRandomInt
    let suitOrdinal = randomSuit % 5
    match suitOrdinal with
    | 0 -> Trump | 1 -> Wand | 2 -> Sword | 3 -> Cup | 4 -> Coin
    | _ -> raise (invalidOp "RNG machine broke")
    |> function
    | Trump -> Trump, (randomOrdinal % 22)
    | Wand  -> Wand, (randomOrdinal % 14)
    | Sword -> Sword, (randomOrdinal % 14)
    | Cup   -> Cup, (randomOrdinal % 14)
    | Coin  -> Coin, (randomOrdinal % 14)
    |> Card

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
