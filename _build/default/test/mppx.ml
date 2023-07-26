let test () =
  let%catch n =
    failwith "fuck";
  in

  match n with
  | Ok ok -> ok |> ignore; ()
  | Error err -> Printf.eprintf "error: %s\n" err; ()


let () =
  test ();
  ()
  (* let matr = [%mat [|0.3; 0.5|]] in *)
  (* Array.iter (Printf.printf "val: %f\n") matr *)
  (* Printf.printf "test %f\n" matr *)
