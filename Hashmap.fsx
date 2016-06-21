type 'a HashMap = HashMap of 'a array

let hashcode item size = (item |> Seq.fold(fun acc y -> acc*31 + int y) 0) % size 

let insert key value (HashMap items) = 
  let key = hashcode key 31
  items.[key] <- value
  HashMap items 

let get key (HashMap items) = 
   let hash = hashcode key 31
   printfn "hash=%A" hash
   items.[hash]

let input = HashMap (Array.create 31 "")
insert "adam" "cooleset" input
insert "carly" "not coo" input

get "adam" input
get "carly" input