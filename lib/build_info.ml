(** Build-time metadata — values come from the generated build_info_data module. *)

let version_string () =
  let dirty = if Build_info_data.git_dirty then " (dirty)" else "" in
  Printf.sprintf "discord-agents @ %s%s — built %s"
    Build_info_data.git_commit dirty Build_info_data.build_time
