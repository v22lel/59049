  open Raylib

  type t_tile = {
    num: int;
    src: int;
    dst: int;
    p: float;
  }

  type tile =
    | Unset
    | Set of t_tile

  (*board data setup*)
  let width = 6
  let height = 6
  let tile_size = 150
  let board_pos = (50, 100)
  let board = Array.make (width * height) Unset

  (*board colors setup*)
  let border_color = Color.yellow
  let border_width = 5
  let plain_color = Color.red

  let anim_speed = 0.1

  let flip f x y = f y x

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

  let pos i =
    (i mod width, i / width)

  let xy i =
    let (x, y) = pos i in
    let (bx, by) = board_pos in
    (bx + x * tile_size, by + y * tile_size)


  let lerp_p p (x, y) (x2, y2) =
    (
      x + int_of_float ((float_of_int (x2 - x)) *. p),
      y + int_of_float ((float_of_int (y2 - y)) *. p)
    )

  let rec find_textsize num text_size =
    let text_width = Raylib.measure_text num text_size in
    if text_width > (tile_size - border_width * 2) then find_textsize num (text_size - 4)
    else (text_size, text_width)

  let center_text x y text_size text_width =
    (
      x + (tile_size / 2 - text_width / 2),
      y + (tile_size / 2 - text_size / 2)
    )

  let draw_tile i = function
    | Unset -> ()
    | Set ({ num; src; dst; p } as tile) -> (
      let dst_xy = xy dst in
      let (x, y) = if p <> 1.0 then (
        let new_p = min 1.0 (p +. anim_speed) in
        board.(i) <- Set { tile with p = new_p };
        dst_xy
          |> lerp_p new_p (xy src)

      ) else (
        board.(i) <- Set { tile with src = dst; p = 1.0 };
        dst_xy
      ) in

      let (bg_color, text_color) = gen_colors num in
      let inner_size = tile_size - border_width * 2 in
      Raylib.draw_rectangle (x + border_width) (y + border_width) inner_size inner_size bg_color;

      let num_s = string_of_int num in
      let (tetx_size, text_width) = find_textsize num_s tile_size in
      let (tx, ty) = center_text x y tetx_size text_width in
      Raylib.draw_text num_s tx ty tetx_size text_color;
    )

    let draw_board () =
      let lim = Array.length board - 1 in
      for i = 0 to lim do
        let (x, y) = xy i in
        let (ix, iy) = (x + border_width, y + border_width) in
        let inner_size = tile_size - border_width * 2 in
        Raylib.draw_rectangle x y tile_size tile_size border_color;
        Raylib.draw_rectangle ix iy inner_size inner_size plain_color;
      done;
      Array.iteri draw_tile board

  let spawn_tile num i =
    board.(i) <- Set {
      num = num;
      src = i;
      dst = i;
      p = 1.0;
    }

  let population_factor = 5
  let populate base amt =
    let count = ref 0 in
    let len = Array.length board in
    Array.iteri (fun i tile -> (
      if tile = Unset && !count <> amt then
        if (Random.int_in_range ~min:0 ~max:population_factor) = 0 || (len - i - amt + !count) <= 0 then (
          spawn_tile base i;
          count := !count + 1;
        )
    )) board

  let reset () =
    Array.iteri (fun i _ -> board.(i) <- Unset) board

  let process_row indices base mods =
    let rec collect = function
      | [] -> []
      | idx :: rest -> (
        match board.(idx) with
          | Unset -> collect rest
          | Set t -> (
            let actual_src = if t.p < 1.0 then t.dst else t.src in
            board.(idx) <- Unset;
            { t with src = actual_src } :: collect rest
          )
      )
    in
    let tiles = collect indices in

    let rec merge = function
      | [] -> []
      | x :: y :: rest when x.num = y.num -> {y with num = y.num * base} :: merge rest
      | x :: rest -> x :: merge rest
    in
    let merged = merge tiles in

    mods := (merged <> tiles) || !mods;function
        if tile.dst <> idx then
          mods := (!mods || true);
        board.(idx) <- Set { tile with
          dst = idx;
          p = if tile.src = idx then 1.0 else 0.0
        };
        insert (t_rest, i_rest)
      )
      | (_, _) -> ()
    in
    ignore (insert (merged, indices))

  let indices start step steps =
    let inds = ref [] in
    for k = 0 to (steps - 1) do
      inds := (start + k * step) :: !inds;
    done;
    !inds

  let on_down base mods = (
    for x = 0 to (width - 1) do
      process_row (indices x width height) base mods;
    done
  )

  let on_up base mods = 
    for x = 0 to (width - 1) do
      let top = width * (height - 1) in
      process_row (indices (x + top) (-width) height) base mods;
    done

  let on_right base mods =
    for y = 0 to (height - 1) do
      process_row (indices (y * width) 1 width) base mods;
    done

  let on_left base mods =
    for y = 0 to (height - 1) do
      process_row (indices (y * width + (width -1)) (-1) width) base mods;
    done