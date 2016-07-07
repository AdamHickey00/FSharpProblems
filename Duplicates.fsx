// check list for duplicates 
let hasDuplicates0 x = (x |> List.length) <> (x |> List.distinct |> List.length) 

let hasDuplicates1 x = 
  let rec loop items = function
    | [] -> false 
    | h::_ when items |> List.exists(fun i -> i=h) -> true 
    | h::t -> loop (h::items) t
  loop [] x

let hasDuplicates2 x = 
  let arr = Array.init ((List.max x) + 1) (fun _ -> None)

  let rec loop = function
    | [] -> false 
    | h::_ when arr.[h].IsSome -> true
    | h::t -> 
      arr.[h] <- Some h 
      loop t      
  
  loop x

// examples 
hasDuplicates0 [1; 2; 3; 4; 2]
hasDuplicates0 [1; 2; 3; 4]

hasDuplicates1 [1; 2; 3; 4; 2]
hasDuplicates1 [1; 2; 3; 4]

hasDuplicates2 [1; 2; 3; 4; 2]
hasDuplicates2 [1; 2; 3; 4]