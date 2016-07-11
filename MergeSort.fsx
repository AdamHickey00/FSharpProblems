open System.Linq

// recursive merge sort
let rec combine a b =
  match a,b with 
  | [],_ -> b 
  | _,[] -> a 
  | aH::aT,bH::_ when aH < bH -> aH :: (combine aT b)
  | aH::_,bH::bT -> bH :: (combine a bT) 

let rec mergeSort = function 
  | [] -> []
  | [_] as x -> x 
  | h::t as x -> 
    let half = (x |> List.length)/2 
    let first = x.Take(half) |> List.ofSeq |> mergeSort
    let second = x.Skip(half) |> List.ofSeq |> mergeSort
    combine first second

mergeSort [5; 3]
mergeSort [5;9;2;0;88;9]