let ff a b =
  try Ok (failwith "fucking suka"; ())
  with | Failure err -> Error err | _ -> Error "unknown error"
let tick () =
  let n a =
    try Ok (if a > 0 then failwith "fuck" else failwith "sub")
    with | Failure err -> Error err | _ -> Error "unknown error" in
  match n - 1 with
  | Ok ok -> (ok |> ignore; ())
  | Error err -> (Printf.eprintf "error: %s\n" err; ())
let test () =
  let x a b c =
    try Ok (if a > 0 then failwith "fuck" else ())
    with | Failure err -> Error err | _ -> Error "unknown error" in
  ()
