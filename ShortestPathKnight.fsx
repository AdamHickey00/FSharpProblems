// get the shortest path from one position to the next for a knight on a chess board
let validPosition (r,c) (board: int array array) =
  let maxRowIndex = board.Length - 1
  let maxColIndex = board.[0].Length - 1

  if r > maxRowIndex || c > maxColIndex then
    false 
  else if r < 0 || c < 0 then
    false
  else 
    true

let hasDuplicate x =
  (x |> List.length) <> (x |> List.distinct |> List.length)  

let shortestList x =
  match x |> List.filter(fun (l,p) -> l > 0) with 
  | [] -> []
  | _ as nonEmptyLists -> nonEmptyLists |> List.minBy(fun (l,p) -> l) |> snd 

let shortestPath start finish board = 
  let rec loop startPos endPos path = 
    match startPos with 
    | _ when startPos = endPos -> startPos::path
    | _ when not (validPosition startPos board) -> []
    | _ when path |> hasDuplicate -> []
    | (r, c) -> 
      let p1 = loop (r-1,c-2) endPos (startPos::path)
      let p2 = loop (r-1,c+2) endPos (startPos::path)
      let p3 = loop (r+1,c-2) endPos (startPos::path)
      let p4 = loop (r+1,c+2) endPos (startPos::path)
      let p5 = loop (r+2,c-1) endPos (startPos::path)
      let p6 = loop (r+2,c+1) endPos (startPos::path)
      let p7 = loop (r-2,c-1) endPos (startPos::path)
      let p8 = loop (r-2,c+1) endPos (startPos::path)
 
      [(p1.Length, p1); (p2.Length, p2); (p3.Length, p3); (p4.Length, p4);
       (p5.Length, p5); (p6.Length, p6); (p7.Length, p7); (p8.Length, p8);]
      |> shortestList

  loop finish start []  

let smallBoard = [| [| 0; 1; 2 |]; [| 0; 1; 2 |] |]
let mediumBoard = [| [| 0; 1; 2 |]; [| 0; 1; 2 |]; [| 0; 1; 2 |]; [| 0; 1; 2 |] |]
let largeBoard = [| 
  [| 0; 1; 2; 3|]; // 0 
  [| 0; 1; 2; 3|]; // 1 
  [| 0; 1; 2; 3|]; // 2 
  [| 0; 1; 2; 3|]; // 3 
|]

mediumBoard |> shortestPath (0,0) (3,2) 
// [(0,0); (1,2); (2,0); (3,2)]

largeBoard |> shortestPath (1,1) (1,0)
// [(0,0); (1,1); (2,3); (0,2); (1,0)]