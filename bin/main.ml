open Raylib

let window_w = 1920
let window_h = 1080


let base = 3

let setup () =
  Raylib.init_window window_w window_h "Your getting toiletted broski :wilted_rose:";
  Raylib.set_target_fps 60  ;

  G.images := Some ({
    logo = Raylib.load_texture "logo.png";
  })

let main_btn x y text = Button.create x y 200 50 text Color.brown Color.yellow 20

(* let loop () =
  let reset_board_btn = main_btn 1000 160 "resedd bord" in

  while not (window_should_close ()) do
    let mods = ref false in
    if Raylib.is_key_pressed Key.W then
      Board2.on_up base mods;
    if Raylib.is_key_pressed Key.S then
      Board2.on_down base mods;
    if Raylib.is_key_pressed Key.A then
      Board2.on_left base mods;
    if Raylib.is_key_pressed Key.D then
      Board2.on_right base mods;
    
    if !mods then
      Board2.populate base 2;
    
    if Button.is_clicked reset_board_btn then (
      ignore (Board2.reset ());
      ignore (Board2.populate base 10);
      ()
    );

    begin_drawing ();

    Raylib.draw_rectangle 0 0 window_w window_h Color.blue;
    D.draw_image (G.img ()).logo 0 0 window_w 100;

    Board2.draw_board ();

    Button.draw_all ();
    end_drawing ()
  done;
  close_window () *)



let board = Board3.create_rect_board 3 6 6

let loop () =
  while not (window_should_close ()) do
    begin_drawing ();

    Raylib.draw_rectangle 0 0 window_w window_h Color.blue;
    D.draw_image (G.img ()).logo 0 0 window_w 100;

    Board3.draw_board board;

    Button.draw_all ();
    end_drawing ()
  done;
  close_window ()

let () = setup ()
  |> loop