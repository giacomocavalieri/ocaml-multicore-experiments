let n = try int_of_string Sys.argv.(1) with _ -> 40

let rec fib = function 
  | n when n < 2 -> 1
  | n -> fib (n-1) + fib (n-2)

let main _ = 
  let res = fib n in 
  Printf.printf "fib(%d) = %d\n" n res 

let _ = main ()