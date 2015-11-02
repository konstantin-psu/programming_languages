exception X


let depth = 1000

let rec f x =
  if (x > depth) then
    raise X
  else
    f (x+1)


let rec g x =
  if (x > depth) then
    ()
  else
    g (x+1)

let rec h x =
  if (x > depth)  then
    raise X
  else 
    try h (x+1)
    with exn -> raise exn

let rec k x =
  if (x > depth)  then
    ()
  else 
    try k (x+1) 
    with exn -> raise exn

let main f n =
  let rec loop i =
    if i = 0 then 
      () 
    else
      ((try f 0 with _ -> ());
       loop (i-1)) in
  loop n
	

let _ = 
  match Sys.argv with
  | [|_;"f";count|] -> main f (int_of_string count)
  | [|_;"g";count|] -> main g (int_of_string count)
  | [|_;"h";count|] -> main h (int_of_string count)
  | [|_;"k";count|] -> main k (int_of_string count)
  | _ -> prerr_string "usage: hw4_1 function_name repeat_count"

