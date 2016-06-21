// how many different ways can a girl jump up an n level staircase if she 
// can jump 1, 2, or 3 steps at a time?

// 0 = []
// 1 = [1]
// 2 = [1; 1] [2]
// 3 = [1; 1; 1] [1; 2] [2; 1] [3]
// 4 = [1; 1; 1; 1] [1; 1; 2] [1; 2; 1] [1; 3] [2; 1; 1] [2; 2] [3; 1]

// integer partition2 for 1,2, and 3
let rec partitions = function 
  | 0 -> [[]]
  | 1 -> [[1]]
  | 2 -> [[1; 1]; [2]]
  | _ as n -> 
    [for x in partitions (n-1) -> 1::x]
    @ [for x in partitions (n-2) -> 2::x]
    @ [for x in partitions (n-3) -> 3::x]

partitions 4