open Raylib

type t = {
  x: int;
  y: int;
  w: int;
  h: int;
  text: string;
  color: Color.t;
  text_color: Color.t;
  text_size: int;
  text_pos: int * int;
}

let all_buttons = ref []

let center_text x y w h text text_size =
  let text_width = Raylib.measure_text text text_size in
  print_endline ("text width: " ^ string_of_int text_width);
  (
    x + (w / 2 - text_width / 2),
    y + (h / 2 - text_size / 2)
  )

let create x y w h text color text_color text_size =
  center_text x y w h text text_size
  |> fun text_pos -> { x; y; w; h; color; text; text_color; text_size; text_pos; }
  |> fun b -> all_buttons := b :: !all_buttons; b


let is_inside b x y =
  x >= b.x && y > b.y && (x - b.w <= b.x) && (y - b.h <= b.y)

let is_clicked b =
  Raylib.is_mouse_button_pressed MouseButton.Left &&
  is_inside b (Raylib.get_mouse_x ()) (Raylib.get_mouse_y ())

let draw b = (
  Raylib.draw_rectangle b.x b.y b.w b.h b.color;
  let (tx, ty) = b.text_pos in
  Raylib.draw_text b.text tx ty b.text_size b.text_color
)

let draw_all () =
  List.iter draw !all_buttons