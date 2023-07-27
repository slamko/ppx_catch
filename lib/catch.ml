
type 'a res = ('a, string) result

let throw err =
  raise_notrace @@ Failure err

let ( let* ) o f =
  match o with
  | Ok x -> f x
  | Error err -> Error err

let ( let@ ) o f =
  match o with
  | Ok x -> f x
  | Error err -> failwith err

let (>>|) v f =
  match v with
  | Ok value -> Ok (f value)
  | Error err -> Error err

let (>>=) v f =
  match v with
  | Ok value -> f value
  | Error err -> Error err

let unwrap res =
  match res with
  | Ok res -> res
  | Error err -> failwith err


