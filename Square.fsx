let rec square = function
  | 0 -> 1
  | 1 -> 1
  | _ as n -> square(n-1) + (2*n) - 1

let square2 n = 
  [1..n] |> List.map (fun _ -> n) |> List.sum

// examples
[1..10] |> List.map square 
[1..10] |> List.map square2 
