// create tree structure at specific level from list 

type Input = {
  id: int 
  level: int
  parent: int Option
  title: string 
}

type Output = {
    id : int  
    title : string
    children : Output list
}

// map input to output
let map (x: Input) = {
  id = x.id  
  title = x.title
  children = []   
}

let input = 
 [
  { id = 1; level = 0; parent = None; title = "PEPFAR"; };
  { id = 1; level = 3; parent = Some 3; title = "Gender"; };
  { id = 2; level = 4; parent = Some 1; title = "Male"; };
  { id = 2; level = 1; parent = Some 1; title = "C"; };
  { id = 3; level = 2; parent = Some 2; title = "C.CLC.001"; };
  { id = 3; level = 4; parent = Some 1; title = "Female"; };
  { id = 2; level = 0; parent = None; title = "HEALTH"; }
]

let rec addChild child id (level:int) (nodes: Output list) =  
  match nodes with 
  | [] -> []
  | h::t when level = 0 ->
    if h.id = id then 
      { h with children = child :: h.children } :: t
    else 
      h :: (addChild child id level t)
  | h::t ->
    let added = addChild child id (level - 1) h.children
    let hWithAdded = { h with children = added }
    hWithAdded :: (addChild child id level t)

let makeTree input = 
  let rec loop (y:Input list) acc : Output list = 
    match y with 
    | [] -> acc
    | h::t when h.level = 0 -> loop t (acc @ [(h|>map)])
    | h::t ->
      let child = h |> map
      let parentId = h.parent |> Option.get
      let newAcc = addChild child parentId (h.level - 1) acc   
      loop t newAcc
      
  loop input []

let sorted x = 
  x |> List.sortBy(fun y -> y.level)

// output
input |> sorted |> makeTree
