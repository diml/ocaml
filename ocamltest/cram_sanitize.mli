(** Sanitization of commands output *)

(** Sanitize the output of a command and insert the leading two spaces
    in front of each line. [exit_code] is the exit code of the command,
    if it is non-zero print "[<code>]" at the end. *)
val sanitize : exit_code:int -> in_channel -> out_channel -> unit
