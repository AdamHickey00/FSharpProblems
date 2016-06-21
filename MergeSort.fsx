open System.Linq

// recursive merge sort
let rec combine x y = 
  match x,y with 
  | [],_ -> y 
  | _,[] -> x 
  | xHead::xTail, yHead::yTail ->
    if xHead < yHead then
      xHead :: (combine xTail y)
    else 
      yHead :: (combine x yTail)

let rec mergeSort x = 
  match x with 
  | [] -> []
  | [v] -> [v]
  | _ -> let half = (x |> List.length) / 2
         let first = mergeSort(x.Take(half) |> List.ofSeq)
         let second = mergeSort(x.Skip(half) |> List.ofSeq)
         combine first second  

mergeSort [5; 3]
mergeSort [5;9;2;0;88;9]