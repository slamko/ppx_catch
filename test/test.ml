
let%try ff a b =
  failwith "fucking suka";
  ()

let tick () =
  let%catch n a =
    if a > 0
    then failwith "fuck"
    else failwith "sub"
  in

  match n (-1) with
  | Ok ok -> ok |> ignore; ()
  | Error err -> Printf.eprintf "error: %s\n" err; ()


let test () =
  let%catch x a b c =
   if a > 0
    then failwith "fuck"
    else ()
  in

  ()
