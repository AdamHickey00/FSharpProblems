open System.Linq

type 'a Tree = 
  | Node of 'a*'a Tree*'a Tree
  | Empty

let leaf x = Node(x, Empty, Empty)

// create a balanced binary tree from a sorted list 
let rec balancedTree = function
  | [] -> Empty
  | [h] -> leaf h
  | h::t as x -> 
    let mid = x.Length/2 
    let left = x.Take(mid) |> List.ofSeq |> balancedTree
    let right = x.Skip(mid + 1) |> List.ofSeq |> balancedTree
    let midValue = x.Skip(mid) |> Seq.head

    Node (midValue, left, right)

// example
[1..15] |> balancedTree  

//              8
//       4             12
//    2      6      10       14
//  1  3   5  7    9  11   13  15