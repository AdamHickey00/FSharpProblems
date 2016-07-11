// input: [1; 2]
// output: [[]; [1]; [2]; [1; 2]]

// input: [1; 2; 3]
// output: [[]; [1]; [2]; [3]; [1; 2]; [1; 3]; [2; 3]; [1; 2; 3]]

// input: [1; 2; 3; 4]
// output: [] [1] [2] [3] [4] [1; 2] [1; 3] [1; 4] [2; 3] [2; 4] [3; 4] [1; 2; 3] [1; 2; 4] [2; 3; 4] [1; 2; 3; 4]

let rec subsets = function 
  | [] -> [[]]
  | h::t -> (subsets t) @ [for i in subsets t -> i @ [h]]

// example
[1; 2; 3; 4] |> List.rev |> subsets |> List.sortBy(fun y -> y |> List.length)