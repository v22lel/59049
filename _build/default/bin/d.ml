open Raylib

let draw_image texture x y w h =
  let src_w = float_of_int (Texture.width texture) in
  let src_h = float_of_int (Texture.height texture) in
  let source = Rectangle.create 0.0 0.0 src_w src_h in
  let dest = Rectangle.create (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h) in
  let origin = Vector2.create 0.0 0.0 in
  draw_texture_pro texture source dest origin 0.0 Color.white