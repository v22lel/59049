open Raylib
open Utils

module TileTbl = Hashtbl.Make(struct
  type t = Vec2.t
  let equal = Vec2.equal
  let hash = Vec2.hash
end)


type t = {
  tiles: t_tile TileTbl.t;
  screen_pos: Vec2.t;
  min: Vec2.t;
  max: Vec2.t;
}
and
t_tile =
| Unset of Vec2.t
| Set of t_state * t_behavior
and
t_behavior = {
  data: t_data;
  on_tick: t_ctx -> unit;
  on_draw: t_ctx -> t_draw_ctx -> unit;
  on_move: t_ctx -> t_move -> unit;
  on_msg: t_ctx -> t_msg -> unit;
}
and
t_dir = | Up | Down | Left | Right
and
t_move = {
    dir: t_dir;
    step: Vec2.t;
    target: Vec2.t;
}
and
t_state = {
  src: Vec2.t;
  pos: Vec2.t;
  mutable p: float;
}
and
t_ctx = {
  tile: t_tile;
  data: t_data;
  state: t_state;
  visual_pos: Vec2.t;
  board: t;
  submit: t_action -> unit;
}
and
t_draw_ctx = {
  screen_pos: Vec2.t;
  size: Vec2.t;
  is_moving: bool;
}
and
t_data =
| Num of int
and
t_action = t_action_type
and
t_sender = {
  state: t_state;
  data: t_data;
}
and
t_msg = {
  subject: string;
  sender: t_sender option;
}
and
t_action_type =
| DoNothing
| DoSet of Vec2.t * t_tile
| DoMove of t_tile * Vec2.t
| DoSetBev of t_tile * t_behavior
| DoSendMsg of t_msg

let tile_pos = function
| Unset xy -> xy
| Set (stat, _) -> stat.pos

let create_msg subject =
  { subject; sender = None }

let create_unset xy = Unset xy

let create_tile xy bev =
  Set ({
    pos = xy;
    src = xy;
    p = 0.0;
  }, bev)

let is_tile_unset = function | Unset _ -> true | _ -> false

let set_tile_at board pos tile =
  TileTbl.replace board.tiles pos tile

let set_tile board tile =
  tile_pos tile |> (fun pos -> set_tile_at board pos tile)

let get_tile board pos =
  TileTbl.find board.tiles pos

let get_tile_opt board pos =
  TileTbl.find_opt board.tiles pos

let create_rectangular ?(x=100) ?(y=100) w h =
  let tbl = TileTbl.create (w * h) in
  let board = {tiles=tbl; screen_pos=(x, y); min=(0, 0); max=(w - 1, h - 1);} in
  for fx = 0 to (w - 1) do
    for fy = 0 to (h - 1) do
      set_tile_at board (fx, fy) (create_unset (fx, fy))
    done
  done;
  board

let create_rotated_square ?(x=100) ?(y=100) r =
  let size = 2 * r + 1 in
  let tbl = TileTbl.create (size * size) in
  let board = {
    tiles = tbl; 
    screen_pos = (x, y); 
    min = (0, 0); 
    max = (size - 1, size - 1);
  } in
  let cx = r in
  let cy = r in
  for fx = 0 to (size - 1) do
    for fy = 0 to (size - 1) do
      if abs (fx - cx) + abs (fy - cy) <= r then
        set_tile_at board (fx, fy) (create_unset (fx, fy))
    done
  done;
  board


let tilesize = 150

let to_screen (tx, ty) =
  (tx * tilesize, ty * tilesize)

let draw_grid board =
  let bg_col = Color.red in
  let fg_col = Color.yellow in
  let fg_width = 2 in
  let bg_size = tilesize - 2 * fg_width in
  TileTbl.iter (fun xy _ -> 
    let (x, y) = board.screen_pos |> Vec2.add (to_screen xy) in
    let (bg_x, bg_y) = (x, y) |> Vec2.add (fg_width, fg_width) in
    Raylib.draw_rectangle x y tilesize tilesize fg_col;
    Raylib.draw_rectangle bg_x bg_y bg_size bg_size bg_col
  ) board.tiles


let messages = Queue.create ()

let move_speed = 0.1

let draw_tiles board action_submit =
  TileTbl.iter (fun (x, y) tile ->
    match tile with
    | Unset _ -> ()
    | Set (stat, bev) -> (
      let screen_pos = board.screen_pos |> Vec2.add (to_screen stat.pos) in
      let screen_src = board.screen_pos |> Vec2.add (to_screen stat.src) in
      let (screen_pos, is_moving) = if stat.p <> 1.0 then (
        let new_p = min (stat.p +. move_speed) 1.0 in
        stat.p <- new_p;
        (Vec2.lerp screen_src screen_pos new_p, true)
      ) else
        (screen_pos, false)
      in

      let ctx = {
        tile = tile;
        data = bev.data;
        state = stat;
        visual_pos = stat.pos;
        submit = action_submit;
        board;
      } in

      Queue.iter (fun msg -> bev.on_msg ctx msg) messages;

      bev.on_tick ctx;

      let draw_ctx = {
        screen_pos;
        size = (tilesize, tilesize);
        is_moving;
      } in

      bev.on_draw ctx draw_ctx;
    )
  ) board.tiles

let tick_board board action_submit =
  draw_grid board;
  draw_tiles board action_submit;
  Queue.clear messages

let action_executor board = function
| DoNothing -> ()
| DoMove (tile, target) -> (if target <> (tile_pos tile) then match tile with
  | Unset _ -> set_tile_at board target (Unset target)
  | Set (stat, bev) -> (
    let src = stat.pos in
    set_tile_at board target (Set ({src=src; pos=target; p=0.0;}, bev));
    set_tile_at board src (create_unset src)
  )
)
| DoSet (target, tile) -> set_tile_at board target tile
| DoSendMsg msg -> Queue.add msg messages
| DoSetBev (tile, bev) -> (print_endline "set bev"; match tile with
  | Unset _ -> ()
  | Set (stat, _) -> let pos = tile_pos tile in set_tile_at board pos (Set (stat, bev))
)

let collect_lanes board coord ext rev =
  let ext2 pair =
    let (p1, p2) = pair in
    if ext pair = p1 then p2 else p1
  in
  let module Q = Pqueue.MakeMin(struct
    type t = t_tile
    let compare = (fun a b -> cmp_int (tile_pos a |> ext2) (tile_pos b |> ext2))
  end) in
  let q = Q.create () in
  TileTbl.iter (fun pos t ->   
    if pos |> ext = coord then
      Q.add q t
  ) board.tiles;

  let (outer, inner) = (ref [], ref []) in
  let last = ref (-1) in
  while not (Q.is_empty q) do
    match Q.pop_min q with
      | Some t -> 
        (*basically if theres a gap in between the tiles we split it up*)
        if !last <> -1 && (tile_pos t |> ext2) - !last > 1 then (
          outer := !inner :: !outer;
          inner := [t]
        ) else inner := if rev then !inner @ [t] else t :: !inner
      | None -> assert (because "is_empty was false")
  done;
  !inner ::!outer

let dir_from_step = function
| (0, 1) -> Down
| (0, -1) -> Up
| (1, 0) -> Right
| (-1, 0) -> Left
| _ -> failwith "illegal step"

let process_lane board lane step action_submit =
  let source = ref (0, 0) in  
  let first = ref true in
  List.iteri (fun i t -> (
    match t with
    | Unset pos -> if !first then (first := false; source := pos)
    | Set (stat, bev) -> (
      let pos = if !first then (
        first := false;
        tile_pos t
      ) else (
        !source
      ) in
      (* action_submit (DoMove (t, pos)); *)
      action_executor board (DoMove (t, pos));
      let ctx : t_ctx = {
        tile = t;
        data = bev.data;
        visual_pos = stat.pos;
        state = {stat with pos;};
        board;
        submit = action_submit;
      } in
      let dir = dir_from_step step in
      bev.on_move ctx {dir; step = Vec2.mul_sc step (-1); target=pos;};
      source := pos |> Vec2.add step
    )
  )) lane

let on_up board action_submit =
  for x = fst board.min to (fst board.max) do
    List.iter (fun lane -> process_lane board lane (0, 1) action_submit) (collect_lanes board x fst true)
  done

let on_down board action_submit =
  for x = fst board.min to (fst board.max) do
    List.iter (fun lane -> process_lane board lane (0, -1) action_submit) (collect_lanes board x fst false)
  done

let on_left board action_submit =
  for y = snd board.min to (snd board.max) do
    List.iter (fun lane -> process_lane board lane (1, 0) action_submit) (collect_lanes board y snd true)
  done

let on_right board action_submit =
  for y = snd board.min to (snd board.max) do
    List.iter (fun lane -> process_lane board lane (-1, 0) action_submit) (collect_lanes board y snd false)
  done

let action_clear_board (board : t) submit =
  TileTbl.iter (fun pos _ -> submit (DoSet (pos, (create_unset pos)))) board.tiles

let action_populate_board tile_fac (board : t) submit amt =
  let population_factor = 3 in
  let count = ref 0 in
  TileTbl.iter (fun pos tile -> (
    if is_tile_unset tile && !count <> amt then
      if (Random.int_in_range ~min:0 ~max:population_factor) = 0 then (
        submit (DoSet (pos, (tile_fac pos)));
        count := !count + 1;
      )
  )) board.tiles