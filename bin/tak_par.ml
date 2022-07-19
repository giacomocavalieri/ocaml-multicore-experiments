module Task = Domainslib.Task 

let (num_domains, x, y, z) = try 
  let num_domains = int_of_string Sys.argv.(1) in 
  let x = int_of_string Sys.argv.(2) in
  let y = int_of_string Sys.argv.(3) in 
  let z = int_of_string Sys.argv.(4) in 
  (num_domains, x, y, z)
with _ -> (1, 18, 12, 6)

let rec tak x y z =
  if x > y then
    tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let rec tak_par p x y z =
  let await = Task.await p in
  let async = Task.async p in 
  if x < 20 && y < 20 then tak x y z 
  else if x > y then 
    let a = async (fun _ -> tak_par p (x-1) y z) in
    let b = async (fun _ -> tak_par p (y-1) z x) in
    let c = async (fun _ -> tak_par p (z-1) x y) in
    tak_par p (await a) (await b) (await c)
  else z

let main _ = 
  let num_additional_domains = num_domains - 1 in
  let pool = Task.setup_pool ~num_additional_domains () in 
  let res = Task.run pool (fun _ -> tak_par pool x y z) in 
  Task.teardown_pool pool;
  Printf.printf "tak(%d,%d,%d)=%d" x y z res

let _ = main ()