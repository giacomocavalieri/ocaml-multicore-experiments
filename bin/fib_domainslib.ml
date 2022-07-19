module Task = Domainslib.Task 

let num_domains = try int_of_string Sys.argv.(1) with _ -> 1 
let n = try int_of_string Sys.argv.(2) with _ -> 40

let rec fib = function 
  | n when n < 2 -> 1 
  | n -> fib (n - 1) + fib (n - 2)

let rec fib_par pool = function 
  | n when n < 20 -> fib n 
  | n -> 
    let a = Task.async pool (fun _ -> fib_par pool (n-1)) in 
    let b = Task.async pool (fun _ -> fib_par pool (n-2)) in 
    Task.await pool a + Task.await pool b 

let main _ =
  let num_additional_domains = num_domains - 1 in
  let pool = Task.setup_pool ~num_additional_domains () in 
  let res = Task.run pool (fun _ -> fib_par pool n) in 
  Task.teardown_pool pool;
  Printf.printf "fib(%d)=%d\n" n res 

let _ = main ()
