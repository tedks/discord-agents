(** Explicit inventory refreshes and cached listings. This state never feeds
    launch selection, Discord channel reconciliation, or filesystem cleanup. *)
module Store = Host_project_inventory
module Scan = Host_project_scan

type t = {
  path : string;
  mutable entries : Store.t;
  mutable cache_error : string option;
  mutable load_failed : bool;
  mutable busy : bool;
}
let create ?(load=Store.load) path =
  let entries, cache_error = match load path with
    | Ok entries -> entries, None | Error message -> [], Some message in
  {path; entries; cache_error; load_failed=Option.is_some cache_error; busy=false}
(* A failed startup read is not an authoritative empty cache. Retry only
   that condition while holding the refresh slot; save failures keep their
   already-known in-memory state and are not silently reloaded. *)
let ensure_loaded t =
  if not t.load_failed then Ok ()
  else match Store.load t.path with
    | Error message ->
      t.cache_error <- Some message;
      Error "Project inventory cache is unreadable; repair or remove the cache file before refreshing."
    | Ok entries ->
      t.entries <- entries; t.cache_error <- None; t.load_failed <- false; Ok ()
let roots ~base_directories host = Option.value host.Host.base_directories ~default:base_directories
let hosts ~local_name registry = Host_diagnostics.hosts ~local_name registry
let configured ~local_name registry =
  let hs = hosts ~local_name registry in
  if List.length hs > Store.max_hosts then Error "Project inventory supports at most 64 configured hosts."
  else Ok hs
let refresh ?(save=fun path entries -> Store.save path entries) ~collect ~preflight ~now ~local_name ~registry ~base_directories t name =
  match configured ~local_name registry,
    Host_diagnostics.resolve ~local_name ~registry name with
  | Error e, _ | _, Error e -> Error e
  | Ok hs, Ok host ->
    let roots = roots ~base_directories host in
    if not (Scan.valid_roots roots) then Error "Configure one to 32 absolute project roots (8192 bytes combined) before refreshing this host."
    else if t.busy then Error "A project inventory refresh is already running. Try again shortly."
    else begin
      t.busy <- true;
      Fun.protect ~finally:(fun () -> t.busy <- false) (fun () ->
        match ensure_loaded t with
        | Error e -> Error e
        | Ok () ->
        match preflight t.path with
        | Error _ -> Error "Project inventory cache is not writable under the current disk policy."
        | Ok () ->
          let started_at = now () in
          let result = collect ~host ~roots in
          (* Removed hosts cannot consume cache capacity indefinitely. Pruning
             observations is unrelated to project/worktree cleanup. *)
          let prior = Store.prune t.entries ~configured:hs in
          match Store.record prior ~now:started_at ~host ~roots result with
          | Error e -> Error e
          | Ok next ->
            match save t.path next with
            | Error e -> t.cache_error <- Some e; Error e
            | Ok () ->
              t.entries <- next; t.cache_error <- None;
              let observation = Option.get (Store.find next host) in
              let message = match observation.error with
                | None -> Printf.sprintf "Scan completed: %d project locations recorded."
                    (List.length (Option.get observation.success).projects)
                | Some why -> "Observation failed: " ^ why ^ ". Prior observations retained for this configuration, if any." in
              Ok (`Assoc ["ok", `Bool true; "scan_succeeded", `Bool (Option.is_none observation.error);
                "report", `String ("Host: " ^ Host_diagnostics.display host.name ^ "\n" ^ message ^
                  "\nCached observation only; use list_project_inventory for locations and timestamps.")]))
    end

type identity = Repository of string | Location of string * string
type location = { host : string; project : Scan.project }
let identity loc = match loc.project.repository with
  | Some repo -> Repository repo
  | None -> Location (String.lowercase_ascii loc.host, loc.project.path)
let identity_json = function
  | Repository repo -> `Assoc ["kind", `String "repository"; "repository", `String repo]
  | Location (host,path) -> `Assoc ["kind", `String "location"; "host", `String host; "path", `String path]
let optional_time = function None -> `Null | Some x -> `Float x
let optional_text = function None -> `Null | Some x -> `String x
let max_page_size = 8
let listing ~now ~local_name ~registry ~base_directories ~offset ~limit t =
  if offset < 0 || limit < 1 || limit > max_page_size then Error "offset must be nonnegative and limit must be from 1 to 8."
  else match configured ~local_name registry with
  | Error _ as e -> e
  | Ok hs ->
    let views = List.map (fun host ->
      let v = Store.view t.entries ~now ~host ~roots:(roots ~base_directories host) in
      let v = if host.enabled && Host.is_local host && not (Host.same_host host.name local_name)
        then {v with state="unroutable"} else v in
      host, v) hs in
    let locations = List.concat_map (fun (host, (v:Store.view)) ->
      List.map (fun project -> {host=host.Host.name;project}) v.projects) views
      |> List.sort (fun a b ->
        let c = compare (identity a) (identity b) in
        if c <> 0 then c else compare (String.lowercase_ascii a.host, a.project.path)
          (String.lowercase_ascii b.host, b.project.path)) in
    let total_locations = List.length locations in
    let total_repositories = List.map identity locations |> List.sort_uniq compare |> List.length in
    (* Pagination is over locations, so even a repository with hundreds of
       worktrees is fully accessible. A group may continue on the next page. *)
    let page = List.filteri (fun i _ -> i >= offset && i - offset < limit) locations in
    let groups = List.fold_left (fun acc loc ->
      let key = identity loc in
      match acc with
      | (k, ls)::rest when k=key -> (k,loc::ls)::rest
      | _ -> (key,[loc])::acc) [] page
      |> List.rev_map (fun (key,ls) -> key,List.rev ls) in
    let display = Host_diagnostics.display in
    let host_json (host, (v:Store.view)) = `Assoc [
      "host", `String host.Host.name; "state", `String v.state;
      "attempted_at", optional_time v.attempted_at;
      "last_success_at", optional_time v.last_success_at;
      "error", optional_text v.error; "locations", `Int (List.length v.projects)] in
    let group_json (key,ls) = `Assoc ["identity",identity_json key;
      "locations", `List (List.map (fun l -> `Assoc [
        "host", `String l.host; "name", `String l.project.name;
        "path", `String l.project.path; "is_bare", `Bool l.project.is_bare]) ls)] in
    let host_lines = List.map (fun (h,(v:Store.view)) ->
      "- " ^ display h.Host.name ^ ": " ^ v.state ^
      (match v.last_success_at with None -> "; no successful snapshot"
       | Some at -> Printf.sprintf "; last success %.3f (Unix time)" at) ^
      (match v.attempted_at with None -> "" | Some at -> Printf.sprintf "; last attempt %.3f" at) ^
      (match v.error with None -> "" | Some e -> "; " ^ display e)) views in
    let group_lines = List.concat_map (fun (key,ls) ->
      let title = match key with Repository r -> display r | Location _ -> "Host-local repository" in
      title :: List.map (fun l -> "  " ^ display l.host ^ ": " ^ display l.project.name ^ " — " ^
        display l.project.path ^ (if l.project.is_bare then " (bare)" else "")) ls) groups in
    let next = if offset < total_locations && List.length page < total_locations - offset
      then Some (offset + List.length page) else None in
    let report = String.concat "\n" (
      [Printf.sprintf "Cached project inventory: %d repositories, %d locations." total_repositories total_locations;
       "Observations only; no probes run. States describe the last scan, not current reachability.";
       "Hosts:"] @ host_lines @
      (match t.cache_error with None -> [] | Some e -> ["Cache warning: " ^ display e]) @
      [Printf.sprintf "Locations from offset %d (up to %d); groups may continue on the next page:" offset limit] @ group_lines @
      (match next with None -> [] | Some n -> [Printf.sprintf "Next page: list_project_inventory offset=%d limit=%d" n limit]) @
      ["Inventory locations are not local !start project numbers and do not authorize remote starts or cleanup."]) in
    Ok (`Assoc ["ok", `Bool true; "report", `String report;
      "hosts", `List (List.map host_json views); "groups", `List (List.map group_json groups);
      "total_repositories", `Int total_repositories; "total_locations", `Int total_locations;
      "offset", `Int offset; "limit", `Int limit;
      "next_offset", (match next with None -> `Null | Some n -> `Int n);
      "cache_error", optional_text t.cache_error])
