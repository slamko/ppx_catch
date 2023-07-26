let test () =
  let%catch n = 3 in
  invalid_arg "fuck";
  ()
  


let () =
  match test () with
  | Ok ok -> ok
  | Error err -> Printf.eprintf "error: %s\n" err
  (* let matr = [%mat [|0.3; 0.5|]] in *)
  (* Array.iter (Printf.printf "val: %f\n") matr *)
  (* Printf.printf "test %f\n" matr *)
