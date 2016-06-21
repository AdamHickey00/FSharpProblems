// fibonnaci sequence
let rec fibonacciI = function
  | 0 -> 0 
  | 1 -> 1 
  | _ as n -> fibonacciI (n-1) + fibonacciI (n-2) 

let fibonnaci f n = 
  [1..n] |> List.map f

// memoized faster version
let cachedFib i (acc: int array) f = 
  if acc.[i] = 0 then 
    let result = f i
    acc.[i] <- result 
    result
  else 
    acc.[i]

let rec fibonacciMemoizedI i = 
  let rec loop acc n = 
    match n with 
    | 0 -> 0
    | 1 -> 1 
    | _ ->
      cachedFib (n-1) acc (loop acc) + cachedFib (n-2) acc (loop acc)

  loop (Array.init i (fun x -> 0)) i

let timed (f: int->int list) n = 
  let watch = new System.Diagnostics.Stopwatch()

  // time function run
  watch.Start()
  let result = f n
  watch.Stop()

  watch.ElapsedMilliseconds,result

// timed fibonnacci run, memoized much faster
printfn "fibonacci 1 = %A" (timed (fibonnaci fibonacciI) 40) // 3080 ms
printfn "fibonacci memoized = %A" (timed (fibonnaci fibonacciMemoizedI) 40) // 0 ms