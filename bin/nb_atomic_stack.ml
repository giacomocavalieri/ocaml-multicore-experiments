module AtomicStack : sig 
  type 'a t 
  val make : unit -> 'a t 
  val push : 'a t -> 'a -> unit 
  val pop : 'a t -> 'a option 
end = struct 
  type 'a t = 'a list Atomic.t 
  
  let make _ = Atomic.make []

  let rec push r a = 
    let s = Atomic.get r in 
    if Atomic.compare_and_set r s (a::s) then ()
    else push r a
  
  let rec pop r = 
    let s = Atomic.get r in 
    match s with 
      | [] -> None 
      | x::xs -> 
        if Atomic.compare_and_set r s (xs) then Some x else pop r
end 

let n = try int_of_string Sys.argv.(1) with _ -> 10
let s = AtomicStack.make ()

let rec producer = function
  | 0 -> ()
  | n ->
      AtomicStack.push s n;
      Printf.printf "Produced %d\n" n;
      producer (n-1)

let rec consumer acc = function 
  | 0 -> acc 
  | n -> match AtomicStack.pop s with 
    | None -> Domain.cpu_relax (); consumer acc n 
    | Some a -> 
      Printf.printf "Consumed %d\n" a;
      consumer (acc + a) (n-1)
      
let main _ =
  let open Domain in 
  let p = spawn (fun _ -> producer n) in 
  let c = spawn (fun _ -> consumer 0 n) in
  let res = join c in
  join p;
  assert (res = n * (n + 1) / 2);
  Printf.printf "\nConsumer produced result: %d\n" res

let _ = main ()