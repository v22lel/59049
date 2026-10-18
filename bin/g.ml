open Raylib

type t_images = {
  logo: Raylib.Texture.t
}

let images: t_images option ref = ref None

let img () = match !images with
  | Some i -> i
  | None -> assert false