// pascal's triangle
let rec pascal r c =
  if c < 0 || c > r then 
    0 
  else if c = 0 || c = r then 
    1 
  else 
    pascal (r-1) (c-1) + pascal (r-1) c

let pascals n = 
  [0..(n-1)] 
  |> List.map(fun r -> [0..r] |> List.map(fun c -> pascal r c) )

let pascal2 n = 
  [for r in [0..(n-1)] do 
    for c in [0..r] do 
      yield pascal r c ]

pascal2 5      
let pascalPretty n =
  (pascals n) |> List.map(fun row ->printfn "%A" row)

pascalPretty 6

//      1 
//     1 1
//    1 2 1
//   1 3 3 1
//  1 4 6 4 1
// 1 5 10 10 5 1