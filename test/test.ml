
let%catch ff a b =
  failwith "ok (not really)"
  ()

let tick () =
  let%catch n a =
    if a > 0
    then failwith "fuck"
    else failwith "sub"
  in

  match n (1) with
  | Ok ok -> ok |> ignore; ()
  | Error err -> Printf.eprintf "error: %s\n" err; ()

