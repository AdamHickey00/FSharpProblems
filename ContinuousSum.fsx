// determine if there exists a continuous list of integers in a list that equal a sum 
let hasContinuousSum originalSum x =
  let rec loop acc = function 
    | [] -> false
    | h::t when acc < 0 -> false 
    | h::t when h = acc -> true  
    | h::t -> (loop (acc - h) t) || (loop originalSum t)

  loop originalSum x 

// example
[3; 6; 2] |> hasContinuousSum 8
// true 

[3; 6; 2] |> hasContinuousSum 5
// false 

[11; 21; 3; 4;] |> hasContinuousSum 24
//true

[11; 21; 3; 4;] |> hasContinuousSum 15
// false