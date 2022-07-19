let n = try int_of_string Sys.argv.(1) with _ -> 40

let rec fib = function 
  | n when n < 2 -> 1
  | n -> fib (n-1) + fib (n-2)

let fib_par = function 
  | n when n > 20 -> 
    let open Domain in
    let d1 = spawn (fun _ -> fib (n-1)) in 
    let d2 = spawn (fun _ -> fib (n-2)) in 
    join d1 + join d2
  | _ -> fib n 

let main _ =
  let res = fib_par n in
  Printf.printf "fib(%d) = %d\n" n res

let _ = main ()