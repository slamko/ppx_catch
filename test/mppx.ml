let test () =
  let%catch n a =
    if a > 0
    then failwith "fuck"
    else ()
  in
  (* n (1) *)

  match n (1) with
  | Ok ok -> ok
  | Error err -> failwith err

let%catch dip () =
  test () 

let () =
  (* match dip with *)
  (* | Ok ok -> ok *)
  (* | Error err ->  Printf.eprintf "error: %s\n" err;  *)
  (* test (); *)
  match dip () with
  | Ok ok -> Printf.printf "Dip ok\n" ;
  | Error err -> Printf.printf "%s\n" err ;
  ()
  (* let matr = [%mat [|0.3; 0.5|]] in *)
  (* Array.iter (Printf.printf "val: %f\n") matr *)
  (* Printf.printf "test %f\n" matr *)
