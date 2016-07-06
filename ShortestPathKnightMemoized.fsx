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

let init r = (fun _ -> Array.init r (fun y -> []))

let fromCache board (r,c) path (cache: (int*int) list [] []) f = 
  if board |> validPosition (r,c) then    
    match cache.[r].[c] with 
    | [] -> 
      //printfn "cached1 r=%A, c=%A = %A" r c cache.[r].[c]
      //cache.[r].[c] <- path 
      //printfn "cached path=%A, cache=%A" path cache
      let shortest = f (r,c) path
      cache.[r].[c] <- shortest
      shortest

    | _ as calculatedPath ->
      printfn "returning cached path of %A" calculatedPath 
      // replace if current path is shorter
       
      // if List.length path < List.length calculatedPath then
      //   printfn "replacing path=%A with path=%A" calculatedPath path 
      //   cache.[r].[c] <- path
      //   (r,c)::path
      // else
      //   (r,c)::calculatedPath

      calculatedPath  
  else 
    [] 

let shortestPath originalStart finish (board: int array array) = 
  let rows = board.Length 
  let cols = board.[0].Length
  let cache = Array.init rows (init cols)

  let rec loop (r,c) path = 
    let start = (r,c)
    if start = originalStart then 
      start :: path
    else if path |> hasDuplicate then 
      []    
    else
      let p1 = fromCache board (r-1,c-2) ((r,c)::path) cache loop
      let p2 = fromCache board (r-1,c+2) ((r,c)::path) cache loop
      let p3 = fromCache board (r+1,c-2) ((r,c)::path) cache loop
      let p4 = fromCache board (r+1,c+2) ((r,c)::path) cache loop
      let p5 = fromCache board (r+2,c-1) ((r,c)::path) cache loop
      let p6 = fromCache board (r+2,c+1) ((r,c)::path) cache loop
      let p7 = fromCache board (r-2,c-1) ((r,c)::path) cache loop
      let p8 = fromCache board (r-2,c+1) ((r,c)::path) cache loop

      // let p1 = loop (r-1,c-2) (startPos::path)
      // let p2 = loop (r-1,c+2) (startPos::path)
      // let p3 = loop (r+1,c-2) (startPos::path)
      // let p4 = loop (r+1,c+2) (startPos::path)
      // let p5 = loop (r+2,c-1) (startPos::path)
      // let p6 = loop (r+2,c+1) (startPos::path)
      // let p7 = loop (r-2,c-1) (startPos::path)
      // let p8 = loop (r-2,c+1) (startPos::path)            

      [(p1.Length, p1); (p2.Length, p2); (p3.Length, p3); (p4.Length, p4);
       (p5.Length, p5); (p6.Length, p6); (p7.Length, p7); (p8.Length, p8);]
      |> shortestList

    //cache.[r].[c]

  let t = loop finish []  
  let (r,c) = originalStart
  printfn "cache=%A" cache
  cache.[r].[c]

let smallBoard = [| [| 0; 1; 2 |]; [| 0; 1; 2 |]; [| 0; 1; 2 |] |]
let mediumBoard = [| [| 0; 1; 2 |]; [| 0; 1; 2 |]; [| 0; 1; 2 |]; [| 0; 1; 2 |] |]
let largeBoard = [| 
  [| 0; 1; 2; 3|]; // 0 
  [| 0; 1; 2; 3|]; // 1 
  [| 0; 1; 2; 3|]; // 2 
  [| 0; 1; 2; 3|]; // 3 
|]

smallBoard |> shortestPath (0,0) (1,2)

mediumBoard |> shortestPath (1,1) (2,2)

mediumBoard |> shortestPath (0,0) (3,2) 
// [(0,0); (1,2); (2,0); (3,2)]

//largeBoard |> shortestPath (1,1) (1,0)
largeBoard |> shortestPath (0,0) (0,2)
// [(0,0); (1,2); (0,2)]


let validMoves (r,c) board = 
  let p1 = (r-1,c-2)
  let p2 = (r-1,c+2)
  let p3 = (r+1,c-2)
  let p4 = (r+1,c+2)
  let p5 = (r+2,c-1)
  let p6 = (r+2,c+1)
  let p7 = (r-2,c-1)
  let p8 = (r-2,c+1)

  p1::p2::p3::p4::p5::p6::p7::[p8]
  |> List.filter (fun x -> validPosition x board)

validMoves (2,1) smallBoard  