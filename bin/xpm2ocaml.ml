open Base

let read_lines p =
  let open Caml in
  let o = open_in p in
  let r = ref [] in
  try
    while true do
      r := input_line o :: !r
    done;
    assert false
  with _ ->
    close_in o;
    List.rev !r

let () =
  let lines = read_lines Caml.Sys.argv.(1) in
  let line fmt = Caml.Format.kasprintf Caml.print_endline fmt in
  line "let %s_xpm = [|"
    Caml.Filename.(basename Caml.Sys.argv.(1) |> chop_extension);
  List.iter lines ~f:(fun xpm_line ->
      match String.prefix (String.strip xpm_line) 1 with
      | "\"" ->
          line "%s" (String.substr_replace_all xpm_line ~pattern:"," ~with_:";")
      | _ -> line "(* XPM data generation: droping: %s *)" xpm_line);
  line "|]"
