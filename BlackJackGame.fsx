// black jack 
open System.Linq

type Suit = 
  | Spade
  | Club 
  | Heart
  | Diamond

type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = Rank*Suit
type Deck = Card list 

type BlackJackHand = {
  FirstCard:Card
  SecondCard:Card
  Cards:Card list
}

type BlackJackGame = {
  Deck:Deck
  Dealer: BlackJackHand 
  Players: BlackJackHand list
}

let allCards suit : Card list = [(Two,suit); (Three,suit); (Four,suit); (Five,suit); (Six,suit); (Seven,suit); (Eight,suit); (Nine,suit); (Ten,suit); (Jack,suit); (Queen,suit); (King,suit); (Ace,suit)]
let unshuffled : Card list = (allCards Spade) @ (allCards Club) @ (allCards Heart) @ (allCards Diamond)
let cardAtIndex cardI cards = 
  cards 
  |> List.mapi(fun i v -> (i+1,v))
  |> List.filter(fun (i,_) -> i = cardI)
  |> List.head 
  |> snd

let numbers (rand:System.Random) =
  let rec loop acc = 
    match acc |> List.length with 
    | 52 -> acc 
    | _ -> let number = rand.Next(1, 53)            
           if acc |> List.exists(fun x -> x = number) then 
             loop acc 
           else 
             loop (number::acc)
  loop [] 

let shuffle cards : Card list = 
  let rec loop number acc = 
    match number with 
    | [] -> acc 
    | h::t -> loop t ((cards |> cardAtIndex h)::acc)

  loop (numbers (new System.Random())) []

let shuffleDeck = shuffle unshuffled

let pop deck = ((deck |> List.head), deck.Skip(1) |> List.ofSeq)

let deal numPlayers = 
  let deck = shuffleDeck
  let (dealerCard1, deck) = (deck |> pop)
  let (dealerCard2, deck) = (deck |> pop)

  let rec loop count acc = 
    match count,acc with 
    | 0,_ -> acc
    | _,(hands,deck) -> 
      let (card1, deck) = (deck |> pop)
      let (card2, deck) = (deck |> pop)
      let hand = {FirstCard = card1; SecondCard = card2; Cards = []}
      loop (count-1) ((hand::hands),deck)

  let (playerHands,deck) = loop numPlayers ([],deck)

  {
    Deck = deck
    Dealer = {FirstCard = dealerCard1; SecondCard = dealerCard2; Cards = []}
    Players = playerHands
  }

let initialHand = deal 2
