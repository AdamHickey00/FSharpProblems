// d -> [d]
// ed -> [e; d] [d; e]
// ted -> [t; e; d] [t; e; d] [t; d; e]

// all permutations of string
let rec distribute e = function
  | [] -> [[e]] 
  | head::tail as x -> (e::x) :: [for y in distribute e tail -> head :: y]

let rec permutations = function
  | [] -> [[]]
  | head::tail -> (permutations tail) |> List.collect (distribute head)

permutations ("ted" |> List.ofSeq)