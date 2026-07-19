(** Text formatters for MCP tool results. *)

let field name fields =
  List.assoc_opt name fields

let string_field object_name name fields =
  match field name fields with
  | Some (`String value) -> Ok value
  | _ ->
    Error (
      Printf.sprintf "%s.%s must be a string" object_name name
    )

let int_field object_name name fields =
  match field name fields with
  | Some (`Int value) -> Ok value
  | _ ->
    Error (
      Printf.sprintf "%s.%s must be an integer" object_name name
    )

let bool_field_default default name fields =
  match field name fields with
  | Some (`Bool value) -> value
  | _ -> default

let list_field name fields =
  match field name fields with
  | None -> Ok []
  | Some (`List values) -> Ok values
  | Some _ ->
    Error (
      Printf.sprintf "Control API %s field must be an array" name
    )

let format_lines ~field_name ~empty_message ~line_of_item fields =
  match list_field field_name fields with
  | Error _ as error -> error
  | Ok [] -> Ok empty_message
  | Ok items ->
    items
    |> List.mapi line_of_item
    |> List.fold_left (fun acc line ->
      match acc, line with
      | (Error _ as error), _ -> error
      | _, (Error _ as error) -> error
      | Ok lines, Ok line -> Ok (line :: lines))
      (Ok [])
    |> Result.map (fun lines ->
      lines |> List.rev |> String.concat "\n")

let format_response ~field_name ~empty_message ~line_of_item = function
  | `Assoc fields ->
    (match field "error" fields with
     | Some (`String message) -> Error message
     | Some _ -> Error "Control API error field must be a string"
     | None ->
       format_lines ~field_name ~empty_message ~line_of_item fields)
  | _ -> Error "Control API response must be an object"

let project_line index = function
  | `Assoc fields ->
    (match string_field "project" "name" fields,
           string_field "project" "path" fields with
     | Ok name, Ok path ->
       let bare_suffix =
         if bool_field_default false "is_bare" fields then " [bare]" else ""
       in
       Ok (Printf.sprintf "%d. **%s** — `%s`%s"
             (index + 1) name path bare_suffix)
     | Error message, _ | _, Error message -> Error message)
  | _ -> Error "project entry must be an object"

let format_list_projects response =
  format_response
    ~field_name:"projects"
    ~empty_message:"No projects found."
    ~line_of_item:project_line
    response

let session_line _index = function
  | `Assoc fields ->
    (match string_field "session" "project_name" fields,
           string_field "session" "agent_kind" fields,
           int_field "session" "message_count" fields,
           string_field "session" "thread_id" fields with
     | Ok project_name, Ok agent_kind, Ok message_count, Ok thread_id ->
       Ok (Printf.sprintf "- **%s** / %s — %d messages (thread: <#%s>)"
             project_name agent_kind message_count thread_id)
     | Error message, _, _, _
     | _, Error message, _, _
     | _, _, Error message, _
     | _, _, _, Error message -> Error message)
  | _ -> Error "session entry must be an object"

let format_list_sessions response =
  format_response
    ~field_name:"sessions"
    ~empty_message:"No active sessions."
    ~line_of_item:session_line
    response
