type read_command_output_from =
  | Stdin
  | File of string

let rewrite_paths =
  let var = "BUILD_PATH_PREFIX_MAP" in
  match Sys.getenv var with
  | exception Not_found -> fun s -> s
  | s -> (
    match Build_path_prefix_map.decode_map s with
    | Error msg ->
      Printf.eprintf "Cannot decode %s: %s\n%!" var msg;
      exit 2
    | Ok map ->
        Cram_lexer.map_paths s ~f:(Build_path_prefix_map.rewrite map))

let sanitize ~exit_code ic oc =
  (try
     while true do
       let line = input_line ic in
       Printf.fprintf oc "  %s\n" (rewrite_paths line)
     done
   with End_of_file -> ());
  if exit_code <> 0 then Printf.printf "  [%d]\n" exit_code
