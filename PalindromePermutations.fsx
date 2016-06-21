let reverse x = 
  x |> List.fold(fun acc y -> y.ToString() + acc) ""

let toString x = 
  x |> List.fold(fun acc y -> acc + y.ToString()) ""

let isPalindrome x = 
  (x |> toString) = (reverse x)    
  
let rec onePermutation c x = 
  match x with 
  | [] -> [[c]] 
  | h::t as original->  
    let rest = [for a in onePermutation c t -> h :: a]
    (c::original) :: rest

let rec permutations x = 
  match x with 
  | [] -> [[]]
  | h::t ->
    printfn "h=%A, t=%A" h t
    List.collect (onePermutation h) (permutations t)

List.map (onePermutation 't') (permutations ['e'; 'd'])
onePermutation 't' ['e'; 'd']
onePermutation 't' ['d'; 'e']

List.map (onePermutation 'e') (permutations ['d'])
onePermutation 'e' ['d']

List.map (onePermutation 'd') (permutations [])
onePermutation 'd' []

permutations ['d'] // [['d']]

permutations ['t'; 'e'; 'd'] 
// [['t'; 'e'; 'd']; ['e'; 't'; 'd']; ['e'; 'd'; 't']; ['t'; 'd'; 'e']; ['d'; 't'; 'e']; ['d'; 'e'; 't']]

// onePerm 't' [['e'; 'd']; ['d'; 'e']]
permutations ['e'; 'd'] // [['e'; 'd']; ['d'; 'e']]

onePermutation 't' 

permutations ("ted" |> List.ofSeq) |> List.exists isPalindrome      