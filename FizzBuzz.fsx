let fizzBuzzi i = 
  match (i%3 = 0),(i%5 = 0) with 
  | true,true -> "FizzBuzz"
  | true,false -> "Fizz"
  | false,true -> "Buzz"
  | false,false -> sprintf "%A" i

let fizzBuzz n = 
  [0..n]
  |> List.map fizzBuzzi

let fizzBuzzConsole n = 
  [for fi in fizzBuzz n -> printfn "%A" fi]

fizzBuzzConsole 20