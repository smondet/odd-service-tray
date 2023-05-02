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
  type t = { mutable main_windows : GWindow.window list } [@@deriving fields]

  let create () = { main_windows = [] }
end

let show_window state =
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
  window#show ();
  State.set_main_windows state (window :: State.main_windows state);
  ()

let () =
  let _ = GMain.init () in
  let state = State.create () in
  let tray_icon =
    (* GMisc.status_icon_from_icon_name "utilities-terminal" *)
    GMisc.status_icon_from_pixbuf (App_icon.as_pixbuf ())
  in
  let menu ~button ~time =
    let entries =
      [
        `I
          ( "Show Main Window",
            fun () ->
              dbgf "Hello";
              show_window state );
        `M ("sub-menu", []);
        `I
          ( "Close All Windows",
            fun () ->
              List.iter (State.main_windows state) ~f:(fun w -> w#destroy ()) );
        `I ("Quit", GMain.quit);
      ]
    in
    GToolbox.popup_menu ~button ~time ~entries
  in
  tray_icon#set_tooltip_text "tooletippe texte";
  ignore
    (tray_icon#connect#activate ~callback:(fun () ->
         (*
            (tray_icon#connct#popup_menu ~callback:(fun a b ->
                 Fmt.epr "a: %d b: %d\n%!" a b;
                 *)
         dbgf "activated %s\n%!" tray_icon#tooltip_text;
         (* menu ~button:1 ~time:182639598l *)
         (* popup () *)
         show_window state
         (* GToolbox.message_box ~title:"testgtk" "Tray icon activated!" *)));
  ignore
    (tray_icon#connect#popup_menu ~callback:(fun a b ->
         dbgf "popup: a: %d b: %d\n%!" a b;
         menu ~button:a ~time:(Int32.of_int_exn b)));
  GMain.main ()
