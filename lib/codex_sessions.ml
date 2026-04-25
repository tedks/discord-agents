(** Discovery of Codex CLI sessions on disk.

    Codex persists every non-ephemeral session as JSONL at:
      ~/.codex/sessions/<YYYY>/<MM>/<DD>/rollout-<ts>-<uuid>.jsonl

    The first line is a [session_meta] envelope carrying the session
    [id], the [cwd] the agent was launched in, and the [instructions]
    string. Subsequent lines are [response_item] envelopes wrapping
    user/assistant message turns.

    We surface [(session_id, working_dir, summary, mtime)] just like
    [Claude_sessions] and [Gemini_sessions] do, so [Bot] and
    [Control_api] can format listings uniformly via
    [format_session_listing]. The summary is the first user message
    that doesn't look like one of the system-context prefaces Codex
    prepends (AGENTS.md, environment_context, instruction wrappers). *)

type info = {
  session_id : string;
  working_dir : string;
  summary : string;
  mtime : float;
}

(** Walk ~/.codex/sessions/<YY>/<MM>/<DD>/, yielding (fpath, mtime)
    for every rollout file newer than [cutoff]. *)
let walk_sessions ?(cutoff=neg_infinity) () =
  let home = Sys.getenv "HOME" in
  let root = Filename.concat home ".codex/sessions" in
  if not (Sys.file_exists root) then []
  else
    let acc = ref [] in
    let safe_readdir path =
      try Array.to_list (Sys.readdir path) with Sys_error _ -> []
    in
    let is_dir path =
      try Sys.is_directory path with Sys_error _ -> false
    in
    List.iter (fun year ->
      let year_path = Filename.concat root year in
      if is_dir year_path then
        List.iter (fun month ->
          let month_path = Filename.concat year_path month in
          if is_dir month_path then
            List.iter (fun day ->
              let day_path = Filename.concat month_path day in
              if is_dir day_path then
                List.iter (fun fname ->
                  if Filename.check_suffix fname ".jsonl"
                     && String.length fname >= 8
                     && String.sub fname 0 8 = "rollout-" then begin
                    let fpath = Filename.concat day_path fname in
                    match (try Some (Unix.stat fpath)
                           with Unix.Unix_error _ -> None) with
                    | Some st when st.Unix.st_mtime > cutoff ->
                      acc := (fpath, st.Unix.st_mtime) :: !acc
                    | _ -> ()
                  end
                ) (safe_readdir day_path)
            ) (safe_readdir month_path)
        ) (safe_readdir year_path)
    ) (safe_readdir root);
    !acc

(** True if [s] starts with one of Codex's system-context prefaces.
    These are injected as fake "user" messages at the top of every
    session and would make terrible summaries if shown verbatim. *)
let looks_like_system_context s =
  let starts_with prefix =
    let n = String.length prefix in
    String.length s >= n && String.sub s 0 n = prefix
  in
  starts_with "# AGENTS.md"
  || starts_with "<environment_context>"
  || starts_with "<INSTRUCTIONS>"
  || starts_with "<user_instructions>"

let truncate n s =
  if String.length s <= n then s else String.sub s 0 n

(** Parse the session file's [session_meta] header (first line) and
    walk subsequent [response_item] lines for the first user message
    that looks like a real prompt rather than injected context.
    Returns [(session_id, cwd, summary)] or None if the file isn't
    a parseable Codex session. *)
let extract_meta fpath =
  try
    let ic = open_in fpath in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      let open Yojson.Safe.Util in
      let line = input_line ic in
      let meta = Yojson.Safe.from_string line in
      if (meta |> member "type" |> to_string_option)
         <> Some "session_meta" then None
      else
        let payload = meta |> member "payload" in
        match payload |> member "id" |> to_string_option with
        | None -> None
        | Some sid ->
          let cwd = payload |> member "cwd" |> to_string_option
                    |> Option.value ~default:"" in
          let summary = ref "" in
          (try
            while !summary = "" do
              let line = input_line ic in
              (try
                let j = Yojson.Safe.from_string line in
                if j |> member "type" |> to_string_option
                   = Some "response_item"
                then
                  let p = j |> member "payload" in
                  if p |> member "type" |> to_string_option = Some "message"
                     && p |> member "role" |> to_string_option = Some "user"
                  then
                    let texts = match p |> member "content" with
                      | `List items ->
                        List.filter_map (fun it ->
                          match it |> member "type" |> to_string_option with
                          | Some "input_text" ->
                            it |> member "text" |> to_string_option
                          | _ -> None) items
                      | _ -> []
                    in
                    let trimmed = String.trim (String.concat " " texts) in
                    if trimmed <> ""
                       && not (looks_like_system_context trimmed)
                    then summary := truncate 80 trimmed
              with _ -> ())
            done
          with End_of_file -> ());
          Some (sid, cwd, !summary))
  with _ -> None

(** Scan for recent Codex sessions, newest first. *)
let discover ?(hours=24) () =
  Eio_unix.run_in_systhread @@ fun () ->
  let cutoff = Unix.gettimeofday () -. (float_of_int hours *. 3600.0) in
  let files = walk_sessions ~cutoff () in
  let infos = List.filter_map (fun (fpath, mtime) ->
    match extract_meta fpath with
    | Some (session_id, working_dir, summary) ->
      Some { session_id; working_dir; summary; mtime }
    | None -> None
  ) files in
  List.sort (fun a b -> compare b.mtime a.mtime) infos

(** Find a session by id prefix across all rollouts.
    Returns [(full_session_id, cwd)] or None. *)
let find_by_prefix session_id_prefix =
  Eio_unix.run_in_systhread @@ fun () ->
  let files = walk_sessions () in
  let prefix_len = String.length session_id_prefix in
  List.find_map (fun (fpath, _) ->
    match extract_meta fpath with
    | Some (sid, wd, _) when String.length sid >= prefix_len
                              && String.sub sid 0 prefix_len = session_id_prefix ->
      Some (sid, wd)
    | _ -> None
  ) files
