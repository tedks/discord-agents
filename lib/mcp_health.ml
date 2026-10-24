(** A minimal, private diagnostic for callback reachability. Do not print the
    health response: it contains session counts and operational diagnostics. *)
let check client =
  match Control_client.request client ~timeout_s:2 "health" with
  | Ok (`Assoc fields) ->
    let oks = List.filter_map (fun (key, value) -> if key = "ok" then Some value else None) fields in
    oks = [`Bool true] && not (List.mem_assoc "error" fields)
  | _ -> false
