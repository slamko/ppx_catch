
let test () =
  let x =
    try fun () -> failwith "suka"
    with | Failure err -> Error err | _ -> Error "unknown error" in
  ()
