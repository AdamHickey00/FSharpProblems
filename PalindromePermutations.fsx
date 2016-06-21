// write a function to determine if any of the permutations of
// a string are a palindrome  

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
    (c::original) :: [for a in onePermutation c t -> h :: a]

let rec permutations x = 
  match x with 
  | [] -> [[]]
  | h::t -> List.collect (onePermutation h) (permutations t)

let permutationPalindromExists x = 
  x 
  |> List.ofSeq
  |> permutations 
  |> List.exists isPalindrome

// test 
permutationPalindromExists "ted" // false
permutationPalindromExists "cviic" // true
