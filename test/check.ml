let test () =
  let n a =
    try Ok (if a > 0 then failwith "fuck" else ())
    with | Failure err -> Error ("n: " ^ err)
    | Invalid_argument err -> Error ("n: " ^ err)
    | _ -> Error "n: unknown error" in
  match n 1 with | Ok ok -> ok | Error err -> failwith err
let dup =
  function
  | Some op ->
      (try Ok op
       with | Failure err -> Error ("dup: " ^ err)
       | Invalid_argument err -> Error ("dup: " ^ err)
       | _ -> Error "dup: unknown error")
  | None ->
      (try Ok (failwith "function does not work")
       with | Failure err -> Error ("dup: " ^ err)
       | Invalid_argument err -> Error ("dup: " ^ err)
       | _ -> Error "dup: unknown error")
let dip () =
  try
    Ok
      (List.map (fun x -> if x > 0 then failwith "Problem" else x + 1)
         [(-1); 0; 0])
  with | Failure err -> Error ("dip: " ^ err)
  | Invalid_argument err -> Error ("dip: " ^ err)
  | _ -> Error "dip: unknown error"
let () =
  match dip () with
  | Ok ok -> Printf.printf "Dip ok\n"
  | Error err -> (Printf.printf "%s\n" err; ())
