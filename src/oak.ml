open Js_of_ocaml
module Color = Color
module Vector = Vector
module Affine = Affine

module Line_cap = struct
  type t = [`Flat | `Round | `Padded]
end

module Line_join = struct
  type t = [`Smooth | `Sharp of float | `Clipped]
end

module Line_style = struct
  type t =
    { color : Color.t
    ; width : float
    ; cap : Line_cap.t
    ; join : Line_join.t
    ; dashing : int array
    ; dash_offset : int
    }

  let default =
    { color = Color.black
    ; width = 1.
    ; cap = `Flat
    ; join = `Sharp 10.
    ; dashing = [||]
    ; dash_offset = 0
    }

  let solid color = { default with color }

  let dashed color = { default with color; dashing = [|8; 4|] }

  let dotted color = { default with color; dashing = [|3; 3|] }
end

module Text_style = struct
  module Alignment = struct
    type t =
      | Start
      | End
      | Left
      | Right
      | Center

    let to_string = function
      | Start -> "start"
      | End -> "end"
      | Left -> "left"
      | Right -> "right"
      | Center -> "center"
  end

  module Baseline = struct
    type t =
      | Top
      | Hanging
      | Middle
      | Alphabetic
      | Ideographic
      | Bottom

    let to_string = function
      | Top -> "top"
      | Hanging -> "hanging"
      | Middle -> "middle"
      | Alphabetic -> "alphabetic"
      | Ideographic -> "ideographic"
      | Bottom -> "bottom"
  end

  module Direction = struct
    type t =
      | Left_to_right
      | Right_to_left
      | Inherit

    let to_string = function
      | Left_to_right -> "ltr"
      | Right_to_left -> "rtl"
      | Inherit -> "inherit"
  end

  module Coloring = struct
    type t =
      | Fill of Color.t
      | Stroke of Color.t
  end

  type t =
    { font      : string
    ; alignment : Alignment.t
    ; baseline  : Baseline.t
    ; direction : Direction.t
    ; coloring  : Coloring.t
    }

  let default =
    { font = "14px sans-serif"
    ; alignment = Start
    ; baseline = Alphabetic
    ; direction = Inherit
    ; coloring = Fill Color.black
    }
end

module Fill_style = struct
  type t =
    | Solid   of Color.t
    | Texture of Image.t
    | Grad    of Color.Gradient.t
end

module Path = struct
  type t = (float * float) array

  let create t = t
end

module Shape = struct
  type t = (float * float) array

  let polygon points = points

  let rect w h =
    let hw = w /. 2. in let hh = h /. 2. in
    [| (-. hw, -. hh); (-. hw, hh); (hw, hh); (hw, -. hh) |]

  let square s = rect s s

  let pi = 4. *. atan 1.

  let oval w h =
    let n  = 50 in
    let t  = 2. *. pi /. float_of_int n in
    let hw = w /. 2. in
    let hh = h /. 2. in
    let f i = let i = float_of_int i in (hw *. cos (t *. i), hh *. sin (t *. i)) in
    Array.init n f

  let circle r = let d = 2. *. r in oval d d

  let ngon n r =
    let t = 2. *. pi /. float_of_int n in
    let f i = let i = float_of_int i in (r *. cos (t *. i), r *. sin (t *. i)) in
    Array.init n f
end

module Form = struct
  open Either

  type basic_form =
    | Raw of (Dom_html.canvasRenderingContext2D Js.t -> unit)
    | Text of Text_style.t * string
    | Path of Line_style.t * Path.t
    | Shape of (Line_style.t, Fill_style.t) Either.t * Shape.t
    | Image of int * int * (int * int) * Image.t (* TODO: Let's see how this works *)
    | Group of Affine.t * [`Array of t array | `Iterator of t Iterator.t ]

  and t =
    { theta : float
    ; scale : float
    ; x     : float
    ; y     : float
    ; alpha : float
    ; form  : basic_form
    }

  let round x : int =
    Js.Unsafe.(meth_call (variable "Math") "round" [|inject (Js.number_of_float x)|])

  let basic form =
    { theta = 0.
    ; scale = 1.
    ; x     = 0.
    ; y     = 0.
    ; alpha = 1.
    ; form
    }

  let raw f = basic (Raw f)

  let fill style shape = basic (Shape (InR style, shape))

  let filled color shape = fill (Fill_style.Solid color) shape

  let textured img shape = fill (Fill_style.Texture img) shape

  let gradient grad shape = fill (Fill_style.Grad grad) shape

  let outlined style shape = basic (Shape (InL style, shape))

  let traced style path = basic (Path (style, path))

  let text style text = basic (Text (style, text))

  let sprite w h pos url = basic (Image (w, h, pos, url))

  let transform_group trans fs = basic (Group (trans, fs))

  let group fs = transform_group Affine.identity fs

  let move (x, y) f = { f with x = f.x +. x; y = f.y +. y }

  let move_x x f = { f with x = f.x +. x }

  let move_y y f = { f with y = f.y +. y }

  let scale s f = { f with scale = f.scale *. s }

  let rotate t f = { f with theta = f.theta +. t }

  let alpha a f = { f with alpha = a }

  module Render_form = struct
    type canvas_ctx = Dom_html.canvasRenderingContext2D Js.t

    let trace (ctx : canvas_ctx)  (path : Path.t) closed =
      if Array.length path > 1
      then begin
        (match path.(0) with (x, y) -> ctx##moveTo x y);
        for i = 1 to Array.length path - 1 do
          match path.(i) with (x, y) -> ctx##lineTo x y
        done;
        if closed
        then match path.(0) with (x, y) -> ctx##lineTo x y
      end
    ;;

    let custom_line_help ctx style path =
      if Array.length path > 1
      then
        let pattern    = style.Line_style.dashing in
        let p_len      = Array.length pattern in
        let (x0, y0)   = path.(Array.length path - 1) in
        let rec loop i (x0, y0) pindex draw =
          if i < 0
          then ()
          else
            let (x1, y1) = path.(i) in
            let dx = x1 -. x0 in let dy = y1 -. y0 in
            let rec loop2 segment_length remaining (dx, dy) draw =
              failwith ""
            in
            failwith ""
  (*           loop2 () (sqrt (dx *. dx +. dy *. dy)) *)
        in
        ctx##moveTo x0 y0;
        failwith ""
    ;;

    let line ctx style path closed =
      if Array.length style.Line_style.dashing = 0 then trace ctx path closed
      else custom_line_help ctx style path;
      ctx##stroke
    ;;

    let draw_line (ctx : canvas_ctx) style path closed =
      ctx##.lineWidth := style.Line_style.width;
      ctx##.lineCap := Js.string (match style.Line_style.cap with
        | `Flat -> "butt" | `Round -> "round" | `Padded -> "square");
      ctx##.lineJoin := Js.string (match style.Line_style.join with
        | `Smooth -> "round" | `Sharp _ -> "miter" | `Clipped -> "bevel");
      ctx##.miterLimit := (match style.Line_style.join with
        | `Sharp x -> x | _ -> 10.);
      (* TODO: Check this *)
      ctx##.strokeStyle := Js.string (Color.to_css_string style.Line_style.color);
      line ctx style path closed
    ;;

    let texture ctx img =
      ctx##createPattern img (Js.string "repeat")
    ;;

    let gradient (ctx : canvas_ctx) grad =
      let g, stops = match grad with
        | Color.Gradient.Linear ((x, y), (a, b), colors) ->
          (ctx##createLinearGradient x (-.y)  a (-.b)), colors
        | Color.Gradient.Radial ((x, y), s, (a, b), t, colors) ->
          (ctx##createRadialGradient x (-.y) s a (-.b) t), colors
      in
      Array.iter (fun (x, c) ->
        g##addColorStop x (Js.string (Color.to_css_string c)))
        stops;
      g
    ;;

    let draw_shape ctx style path =
      trace ctx path false (* TODO: Idk about false here *);
      Fill_style.(match style with
        | Solid color -> ctx##.fillStyle          := Js.string (Color.to_css_string color)
        | Texture img -> ctx##.fillStyle_pattern  := texture ctx img
        | Grad g      -> ctx##.fillStyle_gradient := gradient ctx g);
      ctx##fill
    ;;

    let draw_text (ctx : canvas_ctx)
        { Text_style.font; alignment; baseline; direction; coloring  }
        (text : string)
      =
      ctx##save;
      ctx##scale 1. (-1.);
        ctx##.font := Js.string font;
        ctx##.textAlign := Js.string (Text_style.Alignment.to_string alignment);
        ctx##.textBaseline := Js.string (Text_style.Baseline.to_string baseline);
        (Js.Unsafe.coerce ctx)##.direction := Js.string (Text_style.Direction.to_string direction);
        begin match coloring with
        | Fill color ->
          ctx##.fillStyle := Js.string (Color.to_css_string color);
          ctx##fillText (Js.string text) 0. 0.;
        | Stroke color ->
          ctx##.strokeStyle := Js.string (Color.to_css_string color);
          ctx##strokeText (Js.string text) 0. 0.;
        end;
      ctx##restore
    ;;

    let draw_image ctx (w, h, (src_x, src_y), img) =
      let src_x = float_of_int src_x in let src_y = float_of_int src_y in
      let w = float_of_int w in let h = float_of_int h in
      ctx##drawImage_full img src_x src_y w h 0. 0. w h
    ;;

    let transform (ctx : canvas_ctx) {Affine.translation=(x,y); matrix=(a,b,c,d)} =
      ctx##transform a b c d x y
    ;;

    let make_canvas w h =
      let canvas = Dom_html.(createCanvas document) in
      canvas##.style##.width    := Js.string (Printf.sprintf "%dpx" w);
      canvas##.style##.height   := Js.string (Printf.sprintf "%dpx" h);
      canvas##.style##.display  := Js.string "block";
      canvas##.style##.position := Js.string "absolute";
      canvas##.width := w;
      canvas##.height := h;
      canvas
    ;;

    let rec render_form (ctx : canvas_ctx) {x;y;theta;scale;alpha;form} =
      ctx##save;
        if x <> 0. || y <> 0. then ctx##translate x y;
        if theta <> 0. then ctx##rotate theta;
        if scale <> 1. then ctx##scale scale scale;
        if alpha <> 1. then ctx##.globalAlpha := Js.to_float (ctx##.globalAlpha) *. alpha;
        ctx##beginPath;
        Form.(match form with
          | Raw f -> f ctx
          | Path (style, path)       -> draw_line ctx style path false
          | Text (style, text)       -> draw_text ctx style text
          | Image (w, h, pos, img)   -> draw_image ctx (w, h, pos, img)
          | Shape (InR style, shape) -> draw_shape ctx style shape
          | Shape (InL style, shape) -> draw_line ctx style shape true
          | Group (trans, `Array fs) ->
            (transform ctx trans; Array.iter (render_form ctx) fs)
          | Group (trans, `Iterator fs) ->
            (transform ctx trans; Iterator.iter ~f:(render_form ctx) fs));
      ctx##restore
    ;;
  end

  let render t canvas =
    let ctx = canvas##getContext Dom_html._2d_ in
    let width = float_of_int canvas##.width in
    let height = float_of_int canvas##.height in
    ctx##save;
    begin
      ctx##translate (width /. 2.) (height /. 2.);
      ctx##scale 1. (-1.);
      Render_form.render_form ctx t
    end;
    ctx##restore;
end

