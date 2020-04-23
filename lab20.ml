open Graphics ;;

type bit_map = float list list ;;

class type image_type =
  object
    val img : bit_map
    method no_filter : unit
    method threshold : float -> unit
    method dither : unit
  end ;;

class image (bm : bit_map) : image_type =
  object (self)
    val img = bm

    method private depict (filtered : bit_map) : unit =

      Graphics.open_graph ""; Graphics.clear_graph ();

      let x, y = List.length (List.hd filtered), List.length filtered in
        Graphics.resize_window x y;

      let depict_pix v r c = let lvl = int_of_float (255. *. (1. -. v)) in
        Graphics.set_color (Graphics.rgb lvl lvl lvl);

      plot c (y - r) in
      List.iteri (fun r row ->
                  List.iteri (fun c pix -> depict_pix pix r c) row)
                  filtered;

      ignore (Graphics.read_key ()); Graphics.close_graph ()

    method private filter (f : float -> float) : bit_map =
      List.map (fun row -> List.map f row) img

    method no_filter = self#depict img

    method threshold threshold =
      let new_img = self#filter (fun v -> if v <= threshold then 0. else 1.) in
        self#depict new_img

    method dither =
      let new_img = self#filter
                    (fun v -> if v > Random.float 1. then 1. else 0.) in
        self#depict new_img

  end ;;


(* Show pictures *)

let mona = new image Monalisa.image ;;

mona#no_filter ;;

mona#threshold 0.75 ;;

mona#dither ;;
