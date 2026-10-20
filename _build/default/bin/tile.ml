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
  merge: t_behavior -> t_behavior -> t_behavior option;
}
and
t_action =
| DoNothing
| DoSet of t
| DoSetWith of int * int * (t -> t)
| DoMore of t_action list

let is_unset = function
  | Unset _ -> true
  | _ -> false
  
let pos = function
  | Unset xy -> xy
  | Set (stat, _) -> stat.dst

let create bev xy =
  Set ({ src = xy; dst = xy; p = 1.0; }, bev)

let create_empty xy =
  Unset xy

let action_clear xy =
  DoSet (create_empty xy)

let action_set xy bev =
  DoSet (create bev xy)

let action_move ?(bev_fn=(fun b -> b)) from_xy to_xy =
  if from_xy = to_xy then DoNothing else
  let (from_x, from_y) = from_xy in
  let do_set = DoSetWith (from_x, from_y, (function
    | Unset _ -> Unset to_xy  
    | Set (stat, bev) -> Set ({src = stat.dst; dst = to_xy; p = 0.0;}, bev_fn bev)
  )) in
  DoMore [do_set; action_clear from_xy]