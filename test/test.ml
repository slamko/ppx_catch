
let%try ff a b = ()

let test () =
  let%catch x a b =
     failwith "suka"
  in

  ()
