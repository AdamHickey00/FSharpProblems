// very inefficient bubble sort

// result after each pass
// 2 3 1
// 2 1 3
// 1 2 3

let rec onePass = function
  | [] -> []
  | [y] -> [y] 
  | f::s::tail when f > s -> onePass(s::f::tail)
  | h::tail -> h :: (onePass tail)

let bubbleSort x = 
  let rec loop i acc = 
    match i with 
    | 0 -> acc
    | _ -> loop (i-1) (onePass acc)
  
  loop ((x |> List.length) - 1) (onePass x) 

bubbleSort [2; 3; 1]
bubbleSort [2; 3; 1; 8; 9; 4; 5; 7; 6]