let board = ref []

let to_screen t_x t_y =
  let tilesize = 100 in
  let off_x = 100 in
  let off_y = 100 in
  (off_x + t_x * tilesize, off_y + t_y * tilesize, tilesize)

let rect_i_to_xy w i =
  (i mod w, i / w)

let create_rect_board base w h =
  board := List.init (w * h) (fun i -> rect_i_to_xy w i
    |> Tile.create_empty
  ) 


let draw_board () =
  List.iter (function
    | Tile.Set ({src=(src_x, src_y); dst=(dst_x, dst_y); p=p;}, bev) ->
      let (screen_x, screen_y, tilesize) = to_screen src_x src_y in
      bev.draw screen_x screen_y tilesize tilesize bev.data
    | _ -> ()
  ) !board