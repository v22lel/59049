open Raylib
open Utils

let on_tick (ctx : Board.t_ctx) =
  ()

let rec find_textsize num sx text_size =
  let text_width = Raylib.measure_text num text_size in
  if text_width > sx then find_textsize num sx (text_size - 4)
  else (text_size, text_width)

let center_text x y sx sy text_size text_width =
  (
    x + (sx / 2 - text_width / 2),
    y + (sy / 2 - text_size / 2)
  )

let special_colors cols = function
  | 59049 -> (Color.black, Color.white)
  | _ -> cols

let gen_colors v =
  let bg = if v = 0 then
    Color.create 40 40 50 255
  else
    let r = (v * 37) mod 200 + 55 in
    let g = (v * 73) mod 200 + 55 in
    let b = (v * 151) mod 200 + 55 in
    Color.create r g b 255
  in
  (
    bg,
    Color.create (255 - Color.r bg) (255 - Color.g bg) (255 - Color.b bg) 255
  ) |> flip special_colors v


let ext_data data =
  match data with | Board.Num num -> num

let on_draw (ctx : Board.t_ctx) (d_ctx : Board.t_draw_ctx) =
  let (x, y) = d_ctx.screen_pos in
  let (sx, sy) = d_ctx.size in
  let num = ext_data ctx.data in
  let num_s = string_of_int num in
  let (text_size, text_width) = find_textsize num_s sx sy in
  let (tx, ty) = center_text x y sx sy text_size text_width in
  let (bg_col, txt_col) = gen_colors num in

  Raylib.draw_rectangle x y sx sy bg_col;
  Raylib.draw_text num_s tx ty text_size txt_col

let rec on_msg (ctx : Board.t_ctx) (msg : Board.t_msg) =
  if msg.subject = "decrement" then (
    let num = ext_data ctx.data - 1 in
    ctx.submit (Board.DoSetBev (ctx.tile, create_bev num))
  )

and
on_move (ctx : Board.t_ctx) (mv : Board.t_move) =
  let num = ext_data ctx.data in
  let next_pos = mv.target |> Vec2.add mv.step in

  let handle_other = function
  | Board.Unset _ -> false
  | Board.Set (stat, bev) -> (
    match bev.data with
    | Board.Num o_num when o_num = num -> (
      let new_bev = create_bev (num * 3) in
      let next_t = Board.Set ({stat with pos = ctx.visual_pos;}, new_bev) in
      ctx.submit (Board.DoMove (next_t, next_pos));
      let p = ctx.state.pos in
      ctx.submit (Board.DoSet (p, Board.create_unset p));
      true
    )
    | _ -> false
  )
  in 

  let has_moved = match Board.get_tile_opt ctx.board next_pos with
  | None -> ctx.state.pos <> ctx.visual_pos
  | Some other -> handle_other other
  in
  if has_moved then
    Board.action_populate_board (fun pos -> create_bev 3 |> Board.create_tile pos) ctx.board ctx.submit 1
and

create_bev num : Board.t_behavior = {
  data = Board.Num num;
  on_tick;
  on_draw;
  on_move;
  on_msg;
}