type t = int * int

let add (this_x, this_y) (other_x, other_y) =
  (this_x + other_x, this_y + other_y)

let up n = add (0, n)
let down n = add (0, -n)
let left n = add (-n, 0)
let right n = add (n, 0)

let mul_sc (x, y) s =
  (x * s, y * s)

let lerp (from_x, from_y) (to_x, to_y) t =
  let (dx, dy) = (to_x - from_x, to_y - from_y) in
  let (dx, dy) = (float_of_int dx, float_of_int dy) in
  let (dx, dy) = (dx *. t, dy *. t) in
  let (dx, dy) = (int_of_float dx, int_of_float dy) in
  (from_x + dx, from_y + dy)

let equal (this_x, this_y) (other_x, other_y) =
  this_x = other_x && this_y = other_y

let hash vec2 =
  Hashtbl.hash vec2

let to_string (x, y) =
  "[" ^ (string_of_int x) ^ ";" ^ (string_of_int y) ^ "]"