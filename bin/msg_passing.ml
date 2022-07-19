let r = Atomic.make None 

let sender _ = Atomic.set r (Some "hello!")
let rec receiver _ = match Atomic.get r with 
  | None -> Domain.cpu_relax (); receiver ()
  | Some m -> print_endline m

let main _ = 
  let s = Domain.spawn sender in 
  let r = Domain.spawn receiver in 
  Domain.join s; Domain.join r 

let _ = main ()