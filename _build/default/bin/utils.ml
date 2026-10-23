let rec lst_idx i = function
  | [] -> raise (Invalid_argument "Invalid index")
  | x :: _ when i = 0 -> x
  | x :: rest -> lst_idx (i - 1) rest

let rec lst_idx_range i len = function
  | [] -> raise (Invalid_argument "Invalid range")
  | x :: rest when i = 0 -> if len = 0 then [x] else x :: lst_idx_range 0 (len - 1) rest
  | x :: rest -> lst_idx_range (i - 1) len rest

let cmp_int a b =
  match a, b with
    | a, b when a = b -> 0
    | a, b when a < b -> -1
    | _ -> 1

let because _ =
  false

let flip f x y = f y x

let list_iteri_pair fn lst =
  ()