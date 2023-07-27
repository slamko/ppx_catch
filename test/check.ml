let ff a b =
  try Ok (failwith "ok (not really)" ())
  with | Failure err -> Error ("ff: " ^ err)
  | Invalid_argument err -> Error ("ff: " ^ err)
  | _ -> Error "ff: unknown error"
let tick () =
  let n a =
    try Ok (if a > 0 then failwith "fuck" else failwith "sub")
    with | Failure err -> Error ("n: " ^ err)
    | Invalid_argument err -> Error ("n: " ^ err)
    | _ -> Error "n: unknown error" in
  match n 1 with
  | Ok ok -> (ok |> ignore; ())
  | Error err -> (Printf.eprintf "error: %s\n" err; ())
