// return a list of pairs that total a sum
let rec generatePairs = function
  | h1::h2::[] -> [(h1,h2)]
  | h::t -> (generatePairs t) @ [for i in t -> (h,i)]
  | _ -> failwith "need at least two items in list"

let pairsWithSum sum x = 
  x 
  |> generatePairs
  |> List.filter(fun (p1,p2) -> (p1+p2) = sum)

// example 
[1; 2; 3; 4; 5] |> pairsWithSum 5