let twice_in_parallel f = 
  let open Domain in 
  let d1 = spawn f in 
  let d2 = spawn f in 
  join d1 |> ignore;
  join d2 |> ignore

let plain_ref _ = 
  let r = ref 0 in 
  let f _ = for _ = 1 to 1_000_000 do incr r done in 
  twice_in_parallel f;
  Printf.printf "Non atomic shared ref: %d\n" !r 

let atomic_ref _ = 
  let r = Atomic.make 0 in 
  let f _ = for _ = 1 to 1_000_000 do Atomic.incr r done in 
  twice_in_parallel f;
  Printf.printf "Atomic shared ref: %d\n" (Atomic.get r)

let main _ = 
  plain_ref ();
  atomic_ref () 

let _ = main ()