let n = try int_of_string Sys.argv.(1) with _ -> 40

let rec fib = function 
  | n when n < 2 -> 1
  | n -> fib (n-1) + fib (n-2)

let main _ =
  let open Domain in 
  let d1 = spawn (fun _ -> fib n) in 
  let d2 = spawn (fun _ -> fib n) in 
  let res1 = join d1 in 
  let res2 = join d2 in
  Printf.printf "fib(%d) = %d\n" n res1;
  Printf.printf "fib(%d) = %d\n" n res2

let _ = main ()