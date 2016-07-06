open System.Linq

type 'a Stack = Stack of 'a list 

let empty = Stack []

let push item (Stack items) = Stack (item::items) 

let pop (Stack items) = 
  let popped = items |> List.head
  let newStack = items.Skip(1) |> List.ofSeq
  (popped, Stack newStack)

let min (Stack (items: int list)) = 
  let zipped = items |> List.mapi(fun i v -> (v,i))
  let (min, minIndex) = zipped |> List.minBy(fun (v,_) -> v)

  let newStack = 
    zipped 
    |> List.filter(fun (v,i) -> i <> minIndex) 
    |> List.map(fun (v,_) -> v)
  
  (min, Stack newStack)

// example
empty |> push 1 |> push 3 |> push 4 |> pop |> snd |> push 2 |> push 4 |> min