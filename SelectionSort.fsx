// recursive selection sort

// result after each pass
// 4 2 1 3
// 1 4 2 3
// 1 2 4 3
// 1 2 3 4

let rec min m acc = function 
  | [] -> m,acc
  | h::t when h < m -> min h (m::acc) t 
  | h::t -> min m (h::acc) t    

let rec selectionSort x = 
  match x with 
  | [] -> []
  | h::t -> let (min,theRest) = t |> min h []
            min :: (selectionSort theRest)  

selectionSort [4; 2; 1; 3]