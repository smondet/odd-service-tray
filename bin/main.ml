let dbgf fmt = Format.kasprintf (Printf.eprintf "%s\n%!") fmt

open! Base

module Action = struct
  type t = Command of string [@@deriving sexp, variants, equal, compare]
end

module Service = struct
  type t = {
    display_name : string; [@main]
    start : Action.t;
    stop : Action.t;
    get_status : Action.t;
  }
  [@@deriving sexp, fields, equal, compare, make]

  module Example = struct
    let ml_donkey_docker =
      make "MLDonkey"
        ~start:
          (Action.command
             "docker run --rm -d --name mldonkey-service -v \
              $HOME/.local/mldonkey/config:/var/lib/mldonkey:rw -v \
              $HOME/.local/mldonkey/tmp:/mnt/mldonkey_tmp:rw -e PUID=1000 -e \
              PGID=1000 -v $HOME/Downloads/:/mnt/mldonkey_completed:rw -p \
              4000:4000 -p 4001:4001 -p 4080:4080 -p 20562:20562 -p \
              20566:20566/udp -p 16965:16965 -p 16965:16965/udp -p 6209:6209 \
              -p 6209:6209/udp -p 6881:6881 -p 6882:6882 -p 3617:3617/udp -p \
              4444:4444 -p 4444:4444/udp logicwar/mldonkey")
        ~stop:(Action.command "docker kill mldonkey-service")
        ~get_status:
          (Action.command
             "docker exec  mldonkey-service sh -c 'ps aux | grep mldonkey | \
              grep -v grep'")
  end
end

module Configuration = struct
  type t = {
    default_browser_command : string option;
    services : Service.t list; [@main]
  }
  [@@deriving sexp, fields, equal, compare, make]

  module Example = struct
    let e0 = make [ Service.Example.ml_donkey_docker ]
  end
end

module App_icon = struct
  let data = Data.ost_xpm
  let pixbuf = lazy (GdkPixbuf.from_xpm_data data)
  let as_pixbuf () = Lazy.force pixbuf
end

module State = struct
  type t = {
    mutable configuration : Configuration.t option;
    mutable configuration_path : string option;
    mutable main_windows : GWindow.window list;
  }
  [@@deriving fields, make]

  let create () = make ()

  let load_configuration_path state path =
    let b = Buffer.create 42 in
    let i = Caml.open_in_bin path in
    (try Caml.Buffer.add_channel b i Int.max_value with End_of_file -> ());
    Caml.close_in i;
    let configuration =
      Sexplib.Sexp.of_string (Buffer.contents b) |> Configuration.t_of_sexp
    in
    state.configuration <- Some configuration;
    state.configuration_path <- Some path;
    ()

  let load_example state configuration =
    state.configuration <- Some configuration;
    ()
end

let protect_exn f =
  try f ()
  with e ->
    let msg = Caml.Format.(asprintf "@[Exception:@ %a@]\n%!" Exn.pp e) in
    Caml.Printf.eprintf "%s\n%!" msg;
    GToolbox.message_box ~title:"Error: unhandled exception" msg;
    raise e

let not_implemented s () =
  let msg =
    Caml.Format.(asprintf "@[Not implemented:@ %a@]\n%!" pp_print_text s)
  in
  Caml.Printf.eprintf "%s\n%!" msg;
  GToolbox.message_box ~title:"Error: Not Implemented" msg;
  ()

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

let start_application load_state =
  let _ = GMain.init () in
  let state = State.create () in
  load_state state;
  let tray_icon =
    (* GMisc.status_icon_from_icon_name "utilities-terminal" *)
    GMisc.status_icon_from_pixbuf (App_icon.as_pixbuf ())
  in
  let menu ~button ~time =
    let entries =
      let services_section =
        match State.configuration state with
        | None -> `I ("No Configuration", Fn.ignore)
        | Some conf -> begin
            match Configuration.services conf with
            | [] -> `I ("No services", Fn.ignore)
            | more ->
                `M
                  ( "Services",
                    List.map more ~f:(fun service ->
                        `M
                          ( Service.display_name service,
                            [
                              `I ("Start", not_implemented "start");
                              `I ("Get Status", not_implemented "status");
                              `I ("Stop", not_implemented "stop");
                              `I ("Show UI", not_implemented "show ui");
                            ] )) )
          end
      in
      [
        `I
          ( "Show Main Window",
            fun () ->
              dbgf "Hello";
              show_window state );
        services_section;
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

let () =
  protect_exn @@ fun () ->
  match Array.to_list Caml.Sys.argv with
  | [ _; "start" ] ->
      start_application (fun state ->
          State.load_example state Configuration.Example.e0)
  | [ _; "start"; path ] ->
      start_application (fun state -> State.load_configuration_path state path)
  | [ _; "examples" ] ->
      dbgf "Example-config-0:@ %a" Sexp.pp_hum
        (Configuration.sexp_of_t Configuration.Example.e0)
  | other ->
      Caml.Format.(
        kasprintf failwith "Wrong command: %a"
          (pp_print_list pp_print_string)
          other)
