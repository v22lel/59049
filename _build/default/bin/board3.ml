open Raylib
open Utils

type t_board = {
  mutable ts: Tile.t list;
  xmin: int;
  ymin: int;
  xmax: int;
  ymax: int;
}

let to_screen t_x t_y =
  let tilesize = 100 in
  let off_x = 100 in
  let off_y = 100 in
  (off_x + t_x * tilesize, off_y + t_y * tilesize, tilesize)

let rect_i_to_xy w i =
  (i mod w, i / w)

let create_rect_board base w h =
  let (xmax, ymax) = (w - 1, h - 1) in
  List.init (w * h) (fun i -> rect_i_to_xy w i
    |> Tile.create_empty
  ) |> (fun lst -> { ts = lst; xmin = 0; ymin = 0; xmax; ymax;})

let draw_empty_tile (x, y, ts) =
  let border_w = 2 in
  let bw2 = border_w * 2 in
  Raylib.draw_rectangle x y ts ts Color.red;
  Raylib.draw_rectangle (x + border_w) (y + border_w) (ts - bw2) (ts - bw2) Color.yellow

let draw_board board =
  List.iter (function
    | Tile.Set ({src=(src_x, src_y); dst=(dst_x, dst_y); p=p;}, bev) ->
      let (screen_x, screen_y, tilesize) = to_screen src_x src_y in
      to_screen dst_x dst_y |> draw_empty_tile;
      bev.draw screen_x screen_y tilesize tilesize bev.data
    | Unset (x, y) -> to_screen x y |> draw_empty_tile
  ) board.ts

let collect_lanes board coord ext =
  let module Q = Pqueue.MakeMin(struct
    type t = Tile.t
    let compare = (fun a b -> Utils.cmp_int (Tile.pos a |> ext) (Tile.pos b |> ext))
  end) in
  let q = Q.create () in
  List.iteri (fun i t -> 
    if Tile.pos t |> ext = coord then
      Q.add q t
  ) board.ts;
  let (outer, inner) = (ref [], ref []) in
  let last = ref (-1) in
  while not (Q.is_empty q) do
    match Q.pop_min q with
      | Some t -> 
        (*basically if theres a gap in between the tiles we split it up*)
        if !last <> -1 && (Tile.pos t |> ext) - !last > 1 then (
          outer := !inner :: !outer;
          inner := []
        ) else inner := t :: !inner
      | None -> assert (Utils.because "is_empty was false")
  done;
  !outer