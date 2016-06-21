let reverse x = 
  x |> Seq.fold(fun acc y -> y.ToString() + acc) ""

let isPalindrome x = 
  x = (reverse x)

let isPalindrome2 x = 
  let rec loop a b = 
    match a,b with
    | [],[] -> true 
    | [_],[] -> true 
    | [],[_] -> true
    | ah::at,bh::bt -> 
      if ah = bh then
        loop at bt
      else 
        false 
    | _,_ -> false

  loop (x |> List.ofSeq) ((reverse x) |> List.ofSeq)


let rec distribute e = function
  | [] -> [[e]]
  | h::t as original -> 
    (e::original)::[for xs in distribute e t -> h::xs]

distribute 't' []
// [t] 
distribute 't' ['d']
// [td] :: [dt]

distribute 't' ['e';'d']
// [ted] :: [etd] :: [edt]

let rec adamPerm = function 
  | [] -> [[]] 
  | h::t -> List.collect (distribute h) (adamPerm t)

adamPerm ("ted" |> List.ofSeq) 

// param = "ed"
// e = 't' 
// h = 'e'
// t = 'd'
// original = "ed"
//    ['t'; 'e'; 'd'] :: [['e'; 't'; 'd']; ['e'; 'd'; 't']]
//    // distribute 't' 'd' => [['t'; 'd']; ['d'; 't']] => [['e'; 't'; 'd']; ['e'; 'd'; 't']]                       
// param = "d"
// e = 't'
// h = 'd'
// t = []
// original = "d"
//    [['t'; 'd']; ['d'; 't']]

//distribute 't' ("ed" |> List.ofSeq)

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

// permute "ted"
// distribute 't' [['e'; 'd']; ['d'; 'e']]
// distribute 'e' ['d']   = [['e'; 'd']; ['d'; 'e']]
// distribute 'd' []      = ['d']
// permute []             = []

permute ("ted" |> List.ofSeq)
adamPerm "ted"

let isPalindromePerm x = 
  x 
  |> List.ofSeq
  |> permute
  |> List.exists isPalindrome2


//isPalindromePerm "icvic"