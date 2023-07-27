let test () =
  let%catch n a =
    if a > 0
    then failwith "fuck"
    else ()
  in

  match n (1) with
  | Ok ok -> ok
  | Error err -> failwith err

let%catch dup = function
  | Some op -> op
  | None -> failwith "function does not work"

let%catch dip () =
  List.map (fun x ->
    if x > 0 then failwith "Problem" else x + 1) [-1; 0; 0]

let () =
  match dip () with
  | Ok ok -> Printf.printf "Dip ok\n" ;
  | Error err -> Printf.printf "%s\n" err ;
  ()
