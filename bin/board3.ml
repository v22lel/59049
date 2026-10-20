open Raylib
open Utils

type t_board = {
  mutable ts: Tile.t array;
  xmin: int;
  ymin: int;
  xmax: int;
  ymax: int;
}

let reset board =
  Array.iteri (fun i tile -> board.ts.(i) <- Tile.pos tile |> Tile.create_empty) board.ts

let populate board base amt =
  let population_factor = 5 in

  let slots = Array.to_list board.ts
    |> List.mapi (fun i tile -> (i, tile))
    |> List.filter (fun (_, tile) -> Tile.is_unset tile)
  in
  let count = ref 0 in
  let len = List.length slots in
  List.iteri (fun i (ai, tile) -> (
    if Tile.is_unset tile && !count <> amt then
      if (Random.int_in_range ~min:0 ~max:population_factor) = 0 || (len - i - amt + !count) <= 0 then (
        board.ts.(ai) <- Tile.pos tile |> Tile.create (Num_tile.create_bev base);
        count := !count + 1;
      )
  )) slots

let to_screen t_x t_y =
  let tilesize = 100 in
  let off_x = 100 in
  let off_y = 100 in
  (off_x + t_x * tilesize, off_y + t_y * tilesize, tilesize)

let rect_i_to_xy w i =
  (i mod w, i / w)

let create_rect_board base w h =
  let (xmax, ymax) = (w - 1, h - 1) in
  Array.init (w * h) (fun i -> rect_i_to_xy w i
    |> Tile.create_empty
  ) |> (fun lst -> { ts = lst; xmin = 0; ymin = 0; xmax; ymax;})

let draw_empty_tile (x, y, ts) =
  let border_w = 2 in
  let bw2 = border_w * 2 in
  Raylib.draw_rectangle x y ts ts Color.red;
  Raylib.draw_rectangle (x + border_w) (y + border_w) (ts - bw2) (ts - bw2) Color.yellow

let draw_board board =
  Array.iter (function
    | Tile.Set ({src=(src_x, src_y); dst=(dst_x, dst_y); p=p;}, bev) ->
      let (screen_x, screen_y, tilesize) = to_screen src_x src_y in
      to_screen dst_x dst_y |> draw_empty_tile;
      bev.draw screen_x screen_y tilesize tilesize bev.data
    | Unset (x, y) -> to_screen x y |> draw_empty_tile
  ) board.ts

let collect_lanes board coord ext rev =
  let ext2 pair =
    let (p1, p2) = pair in
    if ext pair = p1 then p2 else p1
  in
  let module Q = Pqueue.MakeMin(struct
    type t = Tile.t
    let compare = (fun a b -> Utils.cmp_int (Tile.pos a |> ext2) (Tile.pos b |> ext2))
  end) in
  let q = Q.create () in
  Array.iteri (fun i t -> 
    if Tile.pos t |> ext = coord then
      Q.add q t
  ) board.ts;
  let (outer, inner) = (ref [], ref []) in
  let last = ref (-1) in
  while not (Q.is_empty q) do
    match Q.pop_min q with
      | Some t -> 
        (*basically if theres a gap in between the tiles we split it up*)
        if !last <> -1 && (Tile.pos t |> ext2) - !last > 1 then (
          outer := !inner :: !outer;
          inner := []
        ) else inner := if rev then !inner @ [t] else t :: !inner
      | None -> assert (Utils.because "is_empty was false")
  done;
  !inner ::!outer

let find_tile board xy =
  let rec loop xy i =
    if i < 0 then -1
    else if Tile.pos board.ts.(i) = xy then i
    else loop xy (i - 1)
  in
  loop xy (Array.length board.ts - 1)

let merge_lane board lane = 
  prerr_endline "merge lane called";
  let unset = ref [] in
  let rec collect = function
    | [] -> []
    | (Tile.Unset _) as t :: rest -> unset := t :: !unset; collect rest
    | (Tile.Set _) as t :: rest -> t :: collect rest
  in
  let sorted = (collect lane) @ !unset in
  let rec merge = function
    | [] -> []
    | (Tile.Set (a_stat, a_bev)) :: ((Tile.Set (b_stat, b_bev))) :: rest -> (
      match a_bev.merge a_bev b_bev with
      | Some mer_bev -> (
        Tile.action_move a_stat.dst b_stat.dst ~bev_fn:(fun _ -> mer_bev)
      ) :: merge rest
      | None -> merge rest
    )
    | t :: rest -> merge rest
  in
  let actions = merge sorted in
  let rec execute lst = 
    List.iter (function
      | Tile.DoNothing -> ()
      | Tile.DoMore more -> execute more
      | Tile.DoSet tile -> (
        let i = Tile.pos tile |> find_tile board in
        board.ts.(i) <- tile;
      )
      | Tile.DoSetWith (x, y, fn) -> (
         let i = (x, y) |> find_tile board in
         board.ts.(i) <- board.ts.(i) |> fn
      )
    ) lst
  in
  execute actions

let on_up board =
  for x = board.xmin to board.xmax do
    List.iter (fun lst -> merge_lane board lst) (collect_lanes board x fst false)
  done