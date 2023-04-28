let dbgf fmt = Format.kasprintf (Printf.eprintf "%s\n%!") fmt

open! Base

module App_icon = struct
  let xpm_hack =
    [|
      "60 60 4 1";
      ". c None";
      "r c #d60000";
      "g c #00c400";
      "w c #FFFFFF";
      ".....wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww";
      "...wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww";
      "..wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww";
      "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww..........";
      "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww........ggggg";
      "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww...................gggggg";
      "wwwwwwwwwwwwwwwwwwww.................................ggggggg";
      "wwwwwwwwwwwwww.......................................ggggggg";
      "wwwwwwwwwww..........................................ggggggg";
      "wwwwwwwwwww..........................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwww...............................................ggggggg";
      "wwwwwwww.............................................ggggggg";
      "wwwwwwwww............................................ggggggg";
      "wwwwwwwww............................................ggggggg";
      "wwwwwwwwww...........................................ggggggg";
      "wwwwwwwwwww..........................................ggggggg";
      "wwwwwwwwwww...........................................gggggg";
      "wwwwwwwwwww............................................ggggg";
      ".wwwwwwwwww.................................................";
      "..wwwwwwwww........rrrrrrrrrrrrrrrrrrrrrr...................";
      "..............rrrrrrrrrrrrrrrrrrrrrrrrrrrrr.................";
      "............rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr...............";
      "............rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr...............";
      "............rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr......wwwwwww..";
      "............rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr.......wwwwwww.";
      "..............rrrrrrrrrrrrrrrrrrrrrrrrrrrrr.........wwwwwwww";
      "..............rrrrrrrrrrrrrrrrrrrrrr................wwwwwwww";
      "....................................................wwwwwwww";
      "gggggg..............................................wwwwwwww";
      "ggggggg.............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg............................................wwwwwwww";
      "gggggggg...........................................wwwwwwwww";
      "gggggggg.........................................wwwwwwwwwww";
      "gggggggg........................................wwwwwwwwwwww";
      "ggggggg..............wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww";
      "gggggg............wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww";
      ".............wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww";
      ".wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww";
      "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww.";
      "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww...";
      "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww.....";
    |]

  let pixbuf = lazy (GdkPixbuf.from_xpm_data xpm_hack)
  let as_pixbuf () = Lazy.force pixbuf
end

module State = struct
  type t = { mutable main_window : GWindow.window option } [@@deriving fields]

  let create () = { main_window = None }
end

let show_window state =
  match State.main_window state with
  | Some s -> s#show ()
  | None ->
      let window =
        GWindow.window ~title:"The Odd Service Tray" ~width:500 ~height:400 ()
          ~resizable:true ~icon:(App_icon.as_pixbuf ())
      in
      (* window#move ~x:20 ~y:20; *)
      (* let _ = window#connect#destroy ~callback:GMain.quit in *)
      let hbox = GPack.hbox ~border_width:10 ~packing:window#add () in
      let button =
        GButton.button ~stock:`QUIT (*  ~label:"Click" *) ~packing:hbox#add ()
      in
      let menu = GButton.button ~label:"Other button" ~packing:hbox#add () in
      let _ = button#connect#clicked ~callback:GMain.quit in
      let _ = menu#connect#clicked ~callback:(fun () -> dbgf "other button") in
      State.set_main_window state (Some window);
      window#show ()

let () =
  let _ = GMain.init () in
  let state = State.create () in
  let tray_icon =
    (* GMisc.status_icon_from_icon_name "utilities-terminal" *)
    GMisc.status_icon_from_pixbuf (App_icon.as_pixbuf ())
  in
  tray_icon#set_tooltip_text "tooletippe texte";
  ignore
    (tray_icon#connect#activate ~callback:(fun () ->
         (*
            (tray_icon#connct#popup_menu ~callback:(fun a b ->
                 Fmt.epr "a: %d b: %d\n%!" a b;
                 *)
         dbgf "activated %s\n%!" tray_icon#tooltip_text;
         (* popup () *)
         show_window state
         (* GToolbox.message_box ~title:"testgtk" "Tray icon activated!" *)));
  GMain.main ()
