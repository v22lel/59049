type t =
| Unset of (int * int)
| Set of t_state * t_behavior
and
t_state = {
  src: int * int;
  dst: int * int;
  p: float;
}
and
t_data =
| Num of int
and
t_behavior = {
  data: t_data;
  draw: int -> int -> int -> int -> t_data -> unit;
  merge: t -> t -> t option;
}

let create bev xy =
  Set ({ src = xy; dst = xy; p = 1.0; }, bev)

let create_empty xy =
  Unset xy