// all permutations of proper parenthesis configurations
 
// input: 1 
// output: ()

// input: 2 
// output: (()) ()()

// input: 3 
// output: ((())) (()()) (())() ()()()

let rec allParenthesis = function
  | 1 -> [['(';')']]
  | _ as i -> 
    let wrapped = [for y in allParenthesis (i-1) -> '(' :: y @ [')']]
    let added = [for y in allParenthesis (i-1) -> y @ ['('; ')']]
    wrapped @ added                  

let toString x = x |> List.fold(fun acc y -> acc + y.ToString()) ""
let pretty x = x |> List.map toString

(allParenthesis 3) |> pretty