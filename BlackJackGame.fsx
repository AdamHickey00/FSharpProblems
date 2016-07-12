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

type Score = 
  | Good of int
  | Bust

type AceScore = {
  AceHigh:Score 
  AceLow:Score
}

let scoreWithAce aceValue (card:Card) =
  match card with 
  | (Two,_) -> 2 | (Three,_) -> 3 | (Four,_) -> 4 | (Five,_) -> 5 | (Six,_) -> 6 | (Seven,_) -> 7 | (Eight,_) -> 8 | (Nine,_) -> 9 | (Ten,_) | (Jack,_) | (Queen,_) | (King,_) -> 10 | (Ace,_) -> aceValue

let scoreAceHigh = scoreWithAce 11
let scoreAceLow = scoreWithAce 1

let score cards = 
  let high = cards |> List.map scoreAceHigh |> List.sum
  let low = cards |> List.map scoreAceLow |> List.sum
  
  match high,low with 
  | _,_ when high > 21 && low > 21 -> {AceHigh=Bust; AceLow=Bust} 
  | _,_ when high > 21 -> {AceHigh=Bust; AceLow=Good low}
  | _,_ -> {AceHigh=Good high; AceLow=Good low}
  
type BlackJackHand = {
  FirstCard:Card
  SecondCard:Card
  Cards:Card list  
} with 
  member this.Score = (this.FirstCard :: this.SecondCard :: this.Cards) |> score 

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
initialHand.Dealer.Score :: (initialHand.Players |> List.map(fun x -> x.Score))
