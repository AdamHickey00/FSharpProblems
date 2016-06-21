open System.Linq

// recursive quick sort
let rec quickSort (x:int list) =
  match x with
  | [] -> []
  | head::tail ->
    let lessThan =
      tail
      |> List.filter (fun y -> y <= head)
      |> quickSort

    let greaterThan =
      tail
      |> List.filter (fun y -> y > head)
      |> quickSort

    lessThan @ [head] @ greaterThan

quickSort [5;9;2;0;88;9]