(* let%try dip () = *)
  (* failwith "try fuck"; *)
  (* () *)

let test () =
  let%catch n a =
    if a > 0
    then
    failwith "fuck";
  in

  match n 3 with
  | Ok ok -> ok |> ignore; ()
  | Error err -> Printf.eprintf "error: %s\n" err; ()

let () =
  (* match dip with *)
  (* | Ok ok -> ok *)
  (* | Error err ->  Printf.eprintf "error: %s\n" err;  *)
  (* test (); *)
  ()
  (* let matr = [%mat [|0.3; 0.5|]] in *)
  (* Array.iter (Printf.printf "val: %f\n") matr *)
  (* Printf.printf "test %f\n" matr *)
