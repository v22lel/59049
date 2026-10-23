open Raylib

let window_w = 1920
let window_h = 1080


let base = 3

let setup () =
  Random.self_init ();
  Raylib.init_window window_w window_h "Your getting toiletted broski :wilted_rose:";
  Raylib.set_target_fps 60  ;

  G.images := Some ({
    logo = Raylib.load_texture "logo.png";
  })

let global_actions = Queue.create ()
let global_submit action =
  Queue.add action global_actions

let loop () =
  (* let board = Board.create_rectangular 6 6 in *)
  let board = Board.create_rotated_square 3 in
  let exec = Board.action_executor board in

  Board.action_populate_board (fun pos -> NumTile.create_bev 3 |> Board.create_tile pos) board global_submit 20;

  while not (window_should_close ()) do  
    if Raylib.is_key_pressed Key.W then (
      Board.on_up board global_submit
    );
    if Raylib.is_key_pressed Key.A then (
      Board.on_left board global_submit
    );
    if Raylib.is_key_pressed Key.S then (
      Board.on_down board global_submit
    );
    if Raylib.is_key_pressed Key.D then (
      Board.on_right board global_submit
    );

    if Raylib.is_key_pressed Key.F then (
      print_endline "sending message";
      global_submit (Board.DoSendMsg (Board.create_msg "decrement"))
    );

    begin_drawing ();

    Raylib.draw_rectangle 0 0 window_w window_h Color.blue;
    D.draw_image (G.img ()).logo 0 0 window_w 100;

    while not (Queue.is_empty global_actions) do
      exec (Queue.pop global_actions)
    done;

    Board.tick_board board global_submit;

    end_drawing ()
  done;
  close_window ()

let () = setup ()
  |> loop