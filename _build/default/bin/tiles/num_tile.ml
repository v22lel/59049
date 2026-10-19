open Raylib

let num_border_width = 2

let rec find_textsize num sx text_size =
  let text_width = Raylib.measure_text num text_size in
  if text_width > (sx - num_border_width * 2) then find_textsize num sx (text_size - 4)
  else (text_size, text_width)

let center_text x y sx sy text_size text_width =
  (
    x + (sx / 2 - text_width / 2),
    y + (sy / 2 - text_size / 2)
  )

let create_bev num : Tile.t_behavior = {
  data = Tile.Num num;
  draw = (fun x y sx sy -> function
  | Tile.Num num -> (
    Raylib.draw_rectangle x y sx sy Color.green
  ));
  merge = (fun a b -> (
    None
  ));
}