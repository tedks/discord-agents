(** Discovery of agy (Antigravity CLI) conversations on disk.

    agy 1.1.3 keeps its app data in ~/.gemini/antigravity-cli/:

    - [brain/<conversation-id>/.system_generated/logs/transcript.jsonl]
      has one JSON object per step. Every conversation has one, and the
      first line is the [USER_INPUT] step whose [content] wraps the
      prompt in [<USER_REQUEST>…</USER_REQUEST>].
    - [cache/last_conversations.json] maps a working directory to the
      most recent conversation started or resumed there.

    Nothing records the working directory of an older conversation:
    [workspace_uris] in [conversation_summaries.db] is empty, and the
    transcripts don't carry it. So a conversation resolves to a
    directory only while it is the latest one for that directory. Any
    other conversation gets [working_dir = ""], is still listed, and
    can't be resumed through discovery. That is the same contract as a
    Gemini session whose projectHash doesn't resolve. *)

type info = {
  session_id : string;
  working_dir : string;  (** "" when the directory isn't recorded *)
  summary : string;
  mtime : float;
}

let default_root () =
  Filename.concat (Sys.getenv "HOME") ".gemini/antigravity-cli"

let transcript_path ~root id =
  Filename.concat (Filename.concat (Filename.concat root "brain") id)
    ".system_generated/logs/transcript.jsonl"

(** agy names conversations by UUID. Requiring that shape keeps stray
    directories under [brain/] out, and keeps [id] safe to put in a
    path. *)
let is_conversation_id s =
  String.length s = 36
  && String.for_all (function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '-' -> true
    | _ -> false) s
  && s.[8] = '-' && s.[13] = '-' && s.[18] = '-' && s.[23] = '-'

let first_line path =
  try
    let ic = open_in path in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      Some (input_line ic))
  with Sys_error _ | End_of_file -> None

(** Index of the first [needle] in [s] at or after [from]. *)
let find_from s ~from needle =
  let n = String.length needle and m = String.length s in
  let rec go i =
    if i + n > m then None
    else if String.sub s i n = needle then Some i
    else go (i + 1)
  in
  if from < 0 then None else go from

let between ~opening ~closing s =
  let find_from i needle = find_from s ~from:i needle in
  match find_from 0 opening with
  | None -> None
  | Some i ->
    let start = i + String.length opening in
    (match find_from start closing with
     | Some j -> Some (String.sub s start (j - start))
     | None -> Some (String.sub s start (String.length s - start)))

(** The bot prepends [<bot-context>…</bot-context>] to the first turn
    of a session (see [Agent_process.compose_session_prompt]); the
    user's words come after it. *)
let strip_bot_context s =
  let s = String.trim s in
  let closing = "</bot-context>" in
  if String.starts_with ~prefix:"<bot-context>" s then
    match find_from s ~from:0 closing with
    | Some i ->
      let rest = i + String.length closing in
      String.trim (String.sub s rest (String.length s - rest))
    | None -> ""
  else s

(** Summary from a transcript's first line: the user's request, without
    agy's wrapper or the bot's context block. "" when the line isn't a
    user-input step. *)
let summary_of_first_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    match json |> member "type" |> to_string_option,
          json |> member "content" |> to_string_option with
    | Some "USER_INPUT", Some content ->
      let request =
        match between ~opening:"<USER_REQUEST>" ~closing:"</USER_REQUEST>"
                content with
        | Some r -> r
        | None -> content
      in
      Resource.normalize_summary ~max_bytes:80 (strip_bot_context request)
    | _ -> ""
  with _ -> ""

(** {conversation id -> working dir} from [last_conversations.json].
    If two directories name the same conversation (it was resumed from a
    second directory), the lexically first directory wins. That keeps
    the result deterministic; either directory is a valid place to
    resume it. *)
let working_dirs ~root =
  let tbl = Hashtbl.create 16 in
  let path = Filename.concat root "cache/last_conversations.json" in
  (try
     match Yojson.Safe.from_file path with
     | `Assoc pairs ->
       List.sort compare pairs
       |> List.iter (fun (dir, v) ->
         match v with
         | `String id
           when is_conversation_id id
                && not (Hashtbl.mem tbl id)
                && String.length dir > 0 && dir.[0] = '/' ->
           Hashtbl.replace tbl id dir
         | _ -> ())
     | _ -> ()
   with _ -> ());
  tbl

(** Conversations whose transcript is newer than [cutoff], as
    (id, transcript path, mtime). *)
let walk ~root ?(cutoff = neg_infinity) () =
  let brain = Filename.concat root "brain" in
  let ids = try Array.to_list (Sys.readdir brain) with Sys_error _ -> [] in
  List.filter_map (fun id ->
    if not (is_conversation_id id) then None
    else
      let path = transcript_path ~root id in
      match Unix.stat path with
      | { Unix.st_kind = Unix.S_REG; st_mtime; _ } when st_mtime > cutoff ->
        Some (id, path, st_mtime)
      | _ -> None
      | exception Unix.Unix_error _ -> None) ids

let discover_in ~root ~hours =
  let cutoff = Unix.gettimeofday () -. (float_of_int hours *. 3600.0) in
  let dirs = working_dirs ~root in
  walk ~root ~cutoff ()
  |> List.map (fun (id, path, mtime) ->
    let summary =
      match first_line path with
      | Some line -> summary_of_first_line line
      | None -> ""
    in
    let working_dir = Option.value (Hashtbl.find_opt dirs id) ~default:"" in
    { session_id = id; working_dir; summary; mtime })
  |> List.sort (fun a b -> compare b.mtime a.mtime)

(** Newest first among conversations that match the prefix, so an
    ambiguous prefix resolves to the most recent one. *)
let find_by_prefix_in ~root prefix =
  if prefix = "" then None
  else
    let dirs = working_dirs ~root in
    walk ~root ()
    |> List.filter (fun (id, _, _) -> String.starts_with ~prefix id)
    |> List.sort (fun (_, _, a) (_, _, b) -> compare b a)
    |> function
    | (id, _, _) :: _ ->
      Some (id, Option.value (Hashtbl.find_opt dirs id) ~default:"")
    | [] -> None

(** Scan for recent agy conversations. Returns newest first. *)
let discover ?(hours = 24) () =
  Eio_unix.run_in_systhread @@ fun () ->
  discover_in ~root:(default_root ()) ~hours

(** Find a conversation by id prefix. Returns (full id, working dir),
    where working dir is "" when it isn't recorded. *)
let find_by_prefix prefix =
  Eio_unix.run_in_systhread @@ fun () ->
  find_by_prefix_in ~root:(default_root ()) prefix
