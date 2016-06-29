// function to validate if parenthesis are matched up correctly
let validateParens x = 
  let rec loop y stack = 
    match y,stack with 
    | [],[] -> true 
    | [],_ -> false 
    | '('::tail,_ -> loop tail ('('::stack)
    | ')'::tail,[] -> false
    | ')'::tail,_::popped -> loop tail popped
    | _ -> failwith "invalid characters"

  loop (x |> List.ofSeq) []

validateParens "((()))()" // true 
validateParens "((())))()" // false