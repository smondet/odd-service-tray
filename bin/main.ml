let dbgf fmt = Format.kasprintf (Printf.eprintf "%s\n%!") fmt
let str = Format.asprintf

open! Base

module Path = struct
  open Stdlib.Filename

  [@@@warning "-32"]

  let quote = quote
  let ( // ) = concat
end

module Component = struct
  type t = Verbatim of string | Format of string
  [@@deriving sexp, variants, equal, compare]

  let t_of_sexp = function
    | Sexp.Atom s -> Verbatim s
    | other -> t_of_sexp other

  let strings l = List.map l ~f:verbatim
end

module Action = struct
  type t = Exec of Component.t list
  [@@deriving sexp, variants, equal, compare]

  let command s =
    exec
      (Component.strings
         (String.split ~on:' ' s |> List.filter ~f:String.(( <> ) "")))

  module Docker = struct
    let start_daemon ~name ?(options = []) ~image command =
      exec
        (Component.strings [ "docker"; "run"; "--rm"; "-d"; "--name"; name ]
        @ options
        @ Component.strings [ image ]
        @ command)

    let volume ?(access = ":rw") l d =
      Component.[ verbatim "-v"; format (str "%s:%s%s" l d access) ]

    let env k v = Component.[ verbatim "-e"; format (str "%s:%s" k v) ]

    let port ?here v =
      Component.
        [ verbatim "-p"; format (str "%s:%s" (Option.value here ~default:v) v) ]

    let udp_port ?here v =
      Component.
        [
          verbatim "-p";
          format (str "%s:%s/udp" (Option.value here ~default:v) v);
        ]

    let kill_daemon ~name = exec (Component.strings [ "docker"; "kill"; name ])

    let get_status ~name =
      exec
        (Component.strings
           [ "docker"; "inspect"; "--format"; "{{json .State.Running}}"; name ])
  end
end

module Status = struct
  type t = Succeeds of Action.t | And of t list
  [@@deriving sexp, variants, equal, compare]
end

module UI = struct
  type t = Web_localhost of { port : int }
  [@@deriving sexp, variants, equal, compare]
end

module Service = struct
  type t = {
    display_name : string; [@main]
    start : Action.t;
    stop : Action.t;
    get_status : Action.t;
    ui : UI.t option;
  }
  [@@deriving sexp, fields, equal, compare, make]

  module Example = struct
    let ml_donkey_docker =
      let name = "ost-ml-donkey" in
      make "MLDonkey"
        ~ui:(UI.web_localhost ~port:4080)
        ~start:
          Action.Docker.(
            start_daemon ~name ~image:"logicwar/mldonkey" []
              ~options:
                (volume "$(HOME)/.local/mldonkey/config" "/var/lib/mldonkey"
                @ volume "$(HOME)/.local/mldonkey/tmp" "/mnt/mldonkey_tmp"
                @ volume "$(HOME)/.local/mldonkey/downloads"
                    "/mnt/mldonkey_completed"
                @ env "PUID" "1000" @ env "PGID" "1000"
                @ List.concat_map ~f:port
                    [
                      "4000";
                      "4001";
                      "4080";
                      "20562";
                      "16965";
                      "6209";
                      "6881";
                      "6882";
                      "4444";
                    ]
                @ List.concat_map ~f:udp_port
                    [ "20566"; "16965"; "6209"; "3617"; "4444" ]))
        ~stop:(Action.Docker.kill_daemon ~name)
        ~get_status:(Action.Docker.get_status ~name)

    let failing_actions =
      make "FailingActions" ~start:(Action.command "exit 3")
        ~stop:
          (Action.exec (Component.strings [ "sh"; "-c"; "sleep 4; exit 4" ]))
        ~get_status:(Action.command "exit 5")
  end
end

module Configuration = struct
  type t = {
    default_browser_command : string option;
    debug_mode : bool; [@default true]
    services : Service.t list; [@main]
  }
  [@@deriving sexp, fields, equal, compare, make]

  module Example = struct
    let e0 = make Service.Example.[ ml_donkey_docker; failing_actions ]
  end
end

module App_icon = struct
  let data = Data.ost_xpm
  let pixbuf = lazy (GdkPixbuf.from_xpm_data data)
  let as_pixbuf () = Lazy.force pixbuf
end

module State = struct
  module Service_data = struct
    type status = (unit, string) Result.t

    type t = {
      service : Service.t; [@main]
      mutable last_status : status;
      mutable status_thread : Thread.t option;
      mutable on_change :
        ([ `Status of status | `Starting | `Stopping ] -> unit) list;
    }
    [@@deriving fields, make]

    let signal_change self change =
      List.iter self.on_change ~f:(fun f -> f change)

    let set_last_status self s =
      List.iter self.on_change ~f:(fun f -> f (`Status s));
      set_last_status self s

    let add_on_change self f = self.on_change <- f :: self.on_change

    let pp fmt { service; last_status; status_thread; on_change = _ } =
      let open Stdlib.Format in
      fprintf fmt "(%s %s -- thread: %s)"
        (Service.display_name service)
        (match last_status with Ok () -> "OK" | Error s -> str "Dead: %s" s)
        (Option.value_map
           ~f:(fun s -> Thread.id s |> Int.to_string_hum)
           ~default:"NONE" status_thread)
  end

  type t = {
    mutable configuration : Configuration.t option;
    mutable configuration_path : string option;
    mutable main_windows : GWindow.window list;
    mutable debug_messages : string list;
    mutable services_state : Service_data.t list;
  }
  [@@deriving fields, make]

  let create () = make ()

  let handle_configuration_change state =
    match configuration state with
    | Some conf ->
        state.services_state <-
          List.map
            ~f:
              (Service_data.make ~last_status:(Error "Not initialized")
                 ?status_thread:None)
            (Configuration.services conf)
    | None -> state.services_state <- []

  let load_configuration_path state path =
    let b = Buffer.create 42 in
    let i = Stdlib.open_in_bin path in
    (try Stdlib.Buffer.add_channel b i Int.max_value with End_of_file -> ());
    Stdlib.close_in i;
    let configuration =
      Sexplib.Sexp.of_string (Buffer.contents b) |> Configuration.t_of_sexp
    in
    state.configuration <- Some configuration;
    state.configuration_path <- Some path;
    handle_configuration_change state;
    ()

  let load_example state configuration =
    state.configuration <- Some configuration;
    handle_configuration_change state;
    ()

  let if_debug state f =
    match configuration state with
    | None | Some { Configuration.debug_mode = false; _ } -> ()
    | _ -> f ()

  let add_debug_message state msg =
    if_debug state (fun () ->
        dbgf "Debug message: %s" msg;
        state.debug_messages <- msg :: state.debug_messages)

  let browser_command state =
    let actual_default = "chromium-browser --user-data-dir=/tmp/ost-bro/" in
    match configuration state with
    | Some { default_browser_command = Some c; _ } -> c
    | _ -> actual_default
end

module Run = struct
  let command_succeeds s =
    match Stdlib.Sys.command s with
    | 0 -> ()
    | other -> failwith (str "Command %S returned: %d" s other)

  let action ~protect state = function
    | Action.Exec comps ->
        let s =
          String.concat ~sep:" "
            (List.map comps
               ~f:
                 Component.(
                   function
                   | Verbatim s -> Path.quote s
                   | Format s ->
                       let b = Buffer.create 42 in
                       let subs = function
                         | "HOME" ->
                             Stdlib.Sys.getenv
                               (if Stdlib.Sys.win32 then "HOMEPATH" else "HOME")
                         | other ->
                             failwith
                               (str "Unknown variable expansion: %S" other)
                       in
                       Stdlib.Buffer.add_substitute b subs s;
                       Path.quote (Buffer.contents b)))
        in
        State.add_debug_message state (str "Running command: %s" s);
        protect (fun () -> command_succeeds s)

  let start_service state srv () =
    State.Service_data.signal_change srv `Starting;
    let service = State.Service_data.service srv in
    action state (Service.start service)

  let stop_service state srv () =
    State.Service_data.signal_change srv `Stopping;
    let service = State.Service_data.service srv in
    action state (Service.stop service)

  let visit_service ~protect state service () =
    protect @@ fun () ->
    match Service.ui service with
    | None ->
        failwith (str "Service %S has no UI" (Service.display_name service))
    | Some (UI.Web_localhost { port }) ->
        command_succeeds
          (str "%s http://localhost:%d" (State.browser_command state) port)

  let start_status_thread state service =
    let rec update_status () =
      dbgf "Service_data: %a" State.Service_data.pp service;
      begin
        try
          action
            ~protect:(fun f -> f ())
            state
            (Service.get_status (State.Service_data.service service));
          State.Service_data.set_last_status service (Ok ())
        with e ->
          State.Service_data.set_last_status service
            (Error (str "Exception: %a" Exn.pp e))
      end;
      Thread.delay 5.;
      update_status ()
    in
    let thd = Thread.create update_status () in
    State.Service_data.set_status_thread service (Some thd);
    ()
end

let protect_exn ?(on_msg = Fn.ignore) f =
  Thread.create
    (fun () ->
      try f ()
      with e ->
        let msg = Stdlib.Format.(asprintf "@[Exception:@ %a@]\n%!" Exn.pp e) in
        Stdlib.Printf.eprintf "%s\n%!" msg;
        on_msg msg;
        (* GToolbox.message_box ~title:"Error: unhandled exception" msg; *)
        raise e)
    ()
  |> Fn.ignore

let _not_implemented s () =
  let msg =
    Stdlib.Format.(asprintf "@[Not implemented:@ %a@]\n%!" pp_print_text s)
  in
  Stdlib.Printf.eprintf "%s\n%!" msg;
  GToolbox.message_box ~title:"Error: Not Implemented" msg;
  ()

let show_window state =
  let window =
    GWindow.window ~title:"The Odd Service Tray" (* ~width:500 ~height:400 *) ()
      ~resizable:true ~icon:(App_icon.as_pixbuf ())
  in
  (* window#move ~x:20 ~y:20; *)
  (* let _ = window#connect#destroy ~callback:GMain.quit in *)
  let main_vbox = GPack.vbox ~packing:window#add () in
  let hbox =
    GPack.button_box `HORIZONTAL ~layout:`START ~packing:main_vbox#add ()
  in
  let button = GButton.button ~stock:`QUIT ~packing:hbox#add () in
  (* let menu = GButton.button ~label:"Other button" ~packing:hbox#add () in *)
  let _ = button#connect#clicked ~callback:GMain.quit in
  let _ = GMisc.separator `HORIZONTAL ~packing:main_vbox#add () in
  let services = State.services_state state in
  let errors_label =
    GMisc.label ~justify:`LEFT ~selectable:true (* ~width:600 *)
      ~line_wrap:true
      ~markup:(str "<b><u>No errors so far…</u></b>")
      ~packing:main_vbox#add ()
  in
  let _ = GMisc.separator `HORIZONTAL ~packing:main_vbox#add () in
  errors_label#set_halign `START;
  let services_table =
    GPack.table
    (* ~border_width:10 *)
    (* ~columns:3 ~rows:(List.length services) *)
      ~packing:main_vbox#add ()
  in
  services_table#set_halign `START;
  List.iteri services
    ~f:
      begin
        fun ith srv ->
          let nth = ref 0 in
          let packing w =
            services_table#attach ~fill:`X ~left:!nth ~top:ith w;
            Int.incr nth
          in
          (* GToolbox.build_menu *)
          let label =
            GMisc.label
              ~justify:`LEFT
                (* ~selectable:true *)
                (* ~width:600 *)
              ~markup:
                (str "<b><u>Service:</u></b>\n <tt>%s</tt>"
                   (State.Service_data.service srv |> Service.display_name))
              ~packing ()
          in
          label#set_halign `START;
          let status_label =
            GMisc.label ~justify:`LEFT (* ~selectable:true *)
              ~width:100 ~markup:"<b><u>???</u></b>" ~packing ()
          in
          let set_markup change =
            status_label#set_label
            @@ str "<tt>%s</tt>"
                 (match change with
                 | `Starting -> "<span foreground=\"orange\">Starting</span>"
                 | `Stopping -> "<span foreground=\"orange\">Stopping</span>"
                 | `Status (Ok ()) -> "<span foreground=\"green\">ON</span>"
                 | `Status (Error _) -> "<span foreground=\"red\">OFF</span>")
          in
          set_markup (`Status (Error ""));
          State.Service_data.add_on_change srv set_markup;
          let start_button = GButton.button ~label:"Start" ~packing () in
          let service = State.Service_data.service srv in
          let exns = ref [] in
          let protect =
            protect_exn ~on_msg:(fun s ->
                exns := s :: !exns;
                errors_label#set_label
                  (str "<b>Error%s</b>%s"
                     (if List.length !exns = 1 then ":" else "s:\n")
                     (String.concat ~sep:"\n"
                        (List.map !exns ~f:(str "⚠ <tt>%s</tt>")))))
          in
          let (_ : GtkSignal.id) =
            start_button#connect#clicked
              ~callback:(Run.start_service ~protect state srv)
          in
          let stop_button = GButton.button ~label:"Stop" ~packing () in
          let (_ : GtkSignal.id) =
            stop_button#connect#clicked
              ~callback:(Run.stop_service ~protect state srv)
          in
          let stop_button = GButton.button ~label:"Show UI" ~packing () in
          let (_ : GtkSignal.id) =
            stop_button#connect#clicked
              ~callback:(Run.visit_service ~protect state service)
          in
          ()
      end;
  (* let _ = menu#connect#clicked ~callback:(fun () -> dbgf "other button") in *)
  window#show ();
  State.set_main_windows state (window :: State.main_windows state);
  ()

let start_application load_state =
  let _ = GMain.init () in
  let state = State.create () in
  load_state state;
  List.iter (State.services_state state) ~f:(Run.start_status_thread state);
  let tray_icon =
    (* GMisc.status_icon_from_icon_name "utilities-terminal" *)
    GMisc.status_icon_from_pixbuf (App_icon.as_pixbuf ())
  in
  let menu ~button ~time =
    let entries =
      let services_section =
        match State.services_state state with
        | [] -> `I ("No services", Fn.ignore)
        | more ->
            `M
              ( "Services",
                List.map more ~f:(fun srv ->
                    let service = State.Service_data.service srv in
                    `M
                      ( Service.display_name service,
                        [
                          `I
                            ( "Start",
                              Run.start_service ~protect:protect_exn state srv
                            );
                          `I
                            ( "Stop",
                              Run.stop_service ~protect:protect_exn state srv );
                          `I
                            ( "Show UI",
                              Run.visit_service ~protect:protect_exn state
                                service );
                        ] )) )
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
         dbgf "activated %s\n%!" tray_icon#tooltip_text;
         show_window state));
  ignore
    (tray_icon#connect#popup_menu ~callback:(fun a b ->
         dbgf "popup: a: %d b: %d\n%!" a b;
         menu ~button:a ~time:(Int32.of_int_exn b)));
  show_window state;
  GMain.main ()

let () =
  (* protect_exn @@ fun () -> *)
  match Array.to_list Stdlib.Sys.argv with
  | [ _; "start" ] ->
      start_application (fun state ->
          State.load_example state Configuration.Example.e0)
  | [ _; "start"; path ] ->
      start_application (fun state -> State.load_configuration_path state path)
  | [ _; "examples" ] ->
      dbgf "Example-config-0:@ %a" Sexp.pp_hum
        (Configuration.sexp_of_t Configuration.Example.e0)
  | other ->
      Stdlib.Format.(
        kasprintf failwith "Wrong command: %a"
          (pp_print_list pp_print_string)
          other)
