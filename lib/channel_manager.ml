(** Discord channel management for project channels.

    Owns the project→channel mapping. Handles case-insensitive matching
    (Discord lowercases channel names), on-demand channel creation,
    channel position bumping, and cleanup of stale channels. *)

module ChannelMap = Map.Make(String) (* project name (original case) -> channel_id *)

type t = {
  mutable channels : string ChannelMap.t;
  mutable category_id : string option;
}

let create () = { channels = ChannelMap.empty; category_id = None }

(** Find a project's channel ID. *)
let find t ~project_name =
  ChannelMap.find_opt project_name t.channels

(** Number of mapped channels. *)
let count t = ChannelMap.cardinal t.channels

(** All channel bindings as (project_name, channel_id) pairs. *)
let bindings t = ChannelMap.bindings t.channels

(** Set up the "Agent Projects" category and map existing channels
    to projects (case-insensitive match on names). *)
let setup ~rest ~guild_id ~projects t =
  if guild_id = "" then
    Logs.info (fun m -> m "channel_manager: no guild_id, skipping")
  else begin
    match Discord_rest.get_guild_channels rest ~guild_id () with
    | Error e ->
      Logs.warn (fun m -> m "channel_manager: failed to get channels: %s" e)
    | Ok channels ->
      let category = List.find_opt (fun (ch : Discord_types.channel) ->
        ch.type_ = Guild_category
        && ch.name = Some "Agent Projects"
      ) channels in
      let cat_id = match category with
        | Some ch ->
          Logs.info (fun m -> m "channel_manager: found category %s" ch.id);
          ch.id
        | None ->
          Logs.info (fun m -> m "channel_manager: creating Agent Projects category");
          match Discord_rest.create_channel rest ~guild_id
                  ~name:"Agent Projects" ~channel_type:4 () with
          | Ok ch -> ch.id
          | Error e ->
            Logs.warn (fun m -> m "channel_manager: failed to create category: %s" e);
            ""
      in
      if cat_id <> "" then begin
        t.category_id <- Some cat_id;
        List.iter (fun (ch : Discord_types.channel) ->
          match ch.parent_id, ch.name with
          | Some pid, Some name when pid = cat_id ->
            (* Case-insensitive match against project names *)
            let matching = List.find_opt (fun (p : Project.t) ->
              String.lowercase_ascii p.name = String.lowercase_ascii name
            ) projects in
            (match matching with
             | Some p ->
               t.channels <- ChannelMap.add p.name ch.id t.channels;
               Logs.info (fun m -> m "channel_manager: mapped %s -> %s" ch.id p.name)
             | None -> ())
          | _ -> ()
        ) channels;
        Logs.info (fun m -> m "channel_manager: %d/%d projects have channels"
          (ChannelMap.cardinal t.channels) (List.length projects))
      end
  end

(** Find or create a channel for a project. Returns the channel ID. *)
let find_or_create ~rest ~guild_id ~project t =
  match find t ~project_name:project.Project.name with
  | Some ch_id -> Some ch_id
  | None ->
    match t.category_id with
    | Some cat_id ->
      let topic = Printf.sprintf "Agent sessions for %s (%s)"
        project.Project.name project.Project.path in
      (match Discord_rest.create_channel rest ~guild_id
               ~name:project.name ~parent_id:cat_id ~topic () with
       | Ok ch ->
         t.channels <- ChannelMap.add project.name ch.id t.channels;
         Logs.info (fun m -> m "channel_manager: created channel for %s" project.name);
         Some ch.id
       | Error e ->
         Logs.warn (fun m -> m "channel_manager: create failed for %s: %s" project.name e);
         None)
    | None -> None

(** Move a project's channel to position 0 (top of category). *)
let bump ~rest ~guild_id ~project_name t =
  match find t ~project_name with
  | None -> ()
  | Some ch_id ->
    if guild_id <> "" then
      ignore (Discord_rest.modify_channel_position rest
        ~guild_id ~channel_id:ch_id ~position:0 ())

(** Delete channels that don't match any current project name.
    Also removes duplicates (keeps first per name). *)
let cleanup ~rest ~guild_id ~projects t =
  if guild_id = "" then Error "no guild_id"
  else
    match Discord_rest.get_guild_channels rest ~guild_id () with
    | Error e -> Error e
    | Ok channels ->
      let project_names = List.map (fun (p : Project.t) ->
        String.lowercase_ascii p.name) projects in
      let seen = Hashtbl.create 32 in
      let to_delete = List.filter (fun (ch : Discord_types.channel) ->
        match ch.parent_id, ch.name, t.category_id with
        | Some pid, Some name, Some cat_id when pid = cat_id ->
          let lname = String.lowercase_ascii name in
          if Hashtbl.mem seen lname then true
          else begin
            Hashtbl.add seen lname true;
            not (List.mem lname project_names)
          end
        | _ -> false
      ) channels in
      let deleted = List.fold_left (fun acc (ch : Discord_types.channel) ->
        match Discord_rest.delete_channel rest ~channel_id:ch.id () with
        | Ok () -> acc + 1
        | Error _ -> acc
      ) 0 to_delete in
      Ok deleted

(** Check if a channel ID belongs to a project channel, return the project name. *)
let project_for_channel t ~channel_id =
  ChannelMap.bindings t.channels
  |> List.find_opt (fun (_, ch_id) -> ch_id = channel_id)
  |> Option.map fst
