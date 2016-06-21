// input: [1; 2]
// output: [[]; [1]; [2]; [1; 2]]

// input: [1; 2; 3]
// output: [[]; [1]; [2]; [3]; [1; 2]; [1; 3]; [2; 3]; [1; 2; 3]]

// input: [1; 2; 3; 4]
// output: [] [1] [2] [3] [4] [1; 2] [1; 3] [1; 4] [2; 3] [2; 4] [3; 4] [1; 2; 3] [1; 2; 4] [2; 3; 4] [1; 2; 3; 4]
let rec loop n x y =
  match n,y with
  | 0,_ -> [[]]
  | _ when n > (x |> List.length) -> []    
  | _,[] -> loop (n+1) x y
  | _,h::t ->    
    [for xs in loop (n-1) x t -> h::xs] @ (loop n x t)

let subsets x = 
  let rec loop2 n = 
    match n with 
    | _ when n > (x |> List.length) -> []
    | _ -> (loop n x x) @ (loop2 (n+1))

  loop2 0

subsets [1; 2; 3; 4]