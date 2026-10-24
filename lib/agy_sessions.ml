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

(** [None] without a HOME: discovery then finds nothing rather than
    raising. *)
let default_root () =
  match Sys.getenv_opt "HOME" with
  | Some home when home <> "" ->
    Some (Filename.concat home ".gemini/antigravity-cli")
  | _ -> None

let transcript_path ~root id =
  Filename.concat (Filename.concat (Filename.concat root "brain") id)
    ".system_generated/logs/transcript.jsonl"

(** agy names conversations by lowercase UUID (8-4-4-4-12). Requiring
    exactly that shape keeps stray directories under [brain/] out, and
    keeps [id] safe to put in a path. *)
let is_conversation_id s =
  String.length s = 36
  && (let ok = ref true in
      String.iteri (fun i c ->
        let dash = i = 8 || i = 13 || i = 18 || i = 23 in
        match c with
        | '-' when dash -> ()
        | ('0' .. '9' | 'a' .. 'f') when not dash -> ()
        | _ -> ok := false) s;
      !ok)

(** First lines longer than this (a huge pasted prompt) aren't read in
    full just to produce an 80-byte summary; such a conversation lists
    with no summary. *)
let max_first_line = 64 * 1024

let first_line path =
  try
    let ic = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      let buf = Buffer.create 4096 in
      let rec go () =
        if Buffer.length buf > max_first_line then None
        else
          match input_char ic with
          | '\n' -> Some (Buffer.contents buf)
          | c -> Buffer.add_char buf c; go ()
          | exception End_of_file ->
            if Buffer.length buf = 0 then None else Some (Buffer.contents buf)
      in
      go ())
  with Sys_error _ -> None

(** Index of the first [needle] in [s] at or after [from]. *)
let find_from s ~from needle =
  let n = String.length needle and m = String.length s in
  let rec matches_at i j = j = n || (s.[i + j] = needle.[j] && matches_at i (j + 1)) in
  let rec go i =
    if i + n > m then None
    else if matches_at i 0 then Some i
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

(** The bot's own wrapping, in the order it is applied to a turn's
    prompt: [<bot-context>…</bot-context>] on a session's first turn
    ([Agent_process.compose_session_prompt]), then the
    [[Discord context: …]] header line on every turn
    ([Agent_runner.build_context_header]). The user's words come after
    both. An unclosed block (truncated input) is left in place rather
    than swallowing the whole summary. *)
let strip_bot_wrapping s =
  let drop_prefix ~opening ~closing s =
    let s = String.trim s in
    if String.starts_with ~prefix:opening s then
      match find_from s ~from:(String.length opening) closing with
      | Some i ->
        let rest = i + String.length closing in
        String.trim (String.sub s rest (String.length s - rest))
      | None -> s
    else s
  in
  s
  |> drop_prefix ~opening:"<bot-context>" ~closing:"</bot-context>"
  |> drop_prefix ~opening:"[Discord context:" ~closing:"]\n"

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
      Resource.normalize_summary ~max_bytes:80 (strip_bot_wrapping request)
    | _ -> ""
  with _ -> ""

(** {conversation id -> working dir} from [last_conversations.json].
    A directory is used only if it is absolute and still exists. If
    several directories name the same conversation (it was resumed from
    a second one), the file doesn't say which was last, so the
    conversation gets no directory: resuming it in the wrong project
    would edit the wrong tree. *)
let working_dirs ~root =
  let dirs = Hashtbl.create 16 in
  let path = Filename.concat root "cache/last_conversations.json" in
  (try
     match Yojson.Safe.from_file path with
     | `Assoc pairs ->
       List.iter (fun (dir, v) ->
         match v with
         | `String id when is_conversation_id id ->
           Hashtbl.replace dirs id
             (dir :: Option.value (Hashtbl.find_opt dirs id) ~default:[])
         | _ -> ()) pairs
     | _ -> ()
   with _ -> ());
  let usable dir =
    String.length dir > 0 && dir.[0] = '/'
    && (try Sys.is_directory dir with Sys_error _ -> false)
  in
  let tbl = Hashtbl.create 16 in
  Hashtbl.iter (fun id ds ->
    match List.sort_uniq compare ds with
    | [dir] when usable dir -> Hashtbl.replace tbl id dir
    | _ -> ()) dirs;
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

(** The conversation whose id starts with [prefix] (case-insensitive,
    since agy ids are lowercase). An ambiguous prefix matches nothing:
    attaching the wrong conversation would bring the wrong context and
    the wrong working directory. The ambiguity is logged. *)
let find_by_prefix_in ~root prefix =
  let prefix = String.lowercase_ascii (String.trim prefix) in
  if prefix = "" then None
  else
    match List.filter (fun (id, _, _) -> String.starts_with ~prefix id)
            (walk ~root ()) with
    | [ (id, _, _) ] ->
      let dirs = working_dirs ~root in
      Some (id, Option.value (Hashtbl.find_opt dirs id) ~default:"")
    | [] -> None
    | many ->
      Logs.info (fun m -> m "agy: prefix %S matches %d conversations; \
                             not resuming any" prefix (List.length many));
      None

(** Scan for recent agy conversations. Returns newest first. *)
let discover ?(hours = 24) () =
  Eio_unix.run_in_systhread @@ fun () ->
  match default_root () with
  | Some root -> discover_in ~root ~hours
  | None -> []

(** Find a conversation by id prefix. Returns (full id, working dir),
    where working dir is "" when it isn't recorded. *)
let find_by_prefix prefix =
  Eio_unix.run_in_systhread @@ fun () ->
  match default_root () with
  | Some root -> find_by_prefix_in ~root prefix
  | None -> None
