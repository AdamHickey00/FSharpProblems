// input: number 
// output: kth number with prime factors of 3 5 and 7

// first 14 numbers with prime factors of 3,5,7
// [105; 315; 525; 735; 945; 1575; 2205; 2835; 4725; 6615; 8505; 14175; 19845; 25515]

let primeFactors x = 
  let rec loop y =    
    match y,y%3 with
    | 1,_ -> 3*5*7
    | 2,2 -> (loop 1) * 3
    | 3,0 -> (loop 1) * 5
    | 4,1 -> (loop 1) * 7    
    | _,2 -> (loop (y-3)) * 3
    | _,0 -> (loop (y-4)) * 5
    | _,1 -> (loop (y-5)) * 7
    | _ -> failwith "invalid case"

  loop x

[1..14] |> List.map primeFactors
