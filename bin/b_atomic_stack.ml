module AtomicStack : sig 
  type 'a t 
  val make : unit -> 'a t 
  val push : 'a t -> 'a -> unit 
  val pop : 'a t -> 'a
end = struct 
  type 'a t = {
    mutable contents : 'a list;
    mutex : Mutex.t;
    condition : Condition.t 
  }

  let make _ = {
    contents = [];
    mutex = Mutex.create ();
    condition = Condition.create ()
  }

  let push r a = 
    Mutex.lock r.mutex;
    r.contents <- a::r.contents;
    Condition.signal r.condition;
    Mutex.unlock r.mutex
  
  let pop r = 
    Mutex.lock r.mutex;
    let rec loop _ = match r.contents with 
      | [] -> Condition.wait r.condition r.mutex; loop ()
      | x::xs -> r.contents <- xs; x in
    let res = loop () in 
    Mutex.unlock r.mutex;
    res 
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
  | n ->
      let a = AtomicStack.pop s in 
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