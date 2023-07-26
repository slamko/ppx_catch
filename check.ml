let ff =
  try Ok (fun a -> fun b -> ())
  with | Failure err -> Error err | _ -> Error "unknown error"
let test () =
  let x =
    try Ok (fun a -> fun b -> failwith "suka")
    with | Failure err -> Error err | _ -> Error "unknown error" in
  ()
