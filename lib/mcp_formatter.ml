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

let int_error object_name name =
  Error (
    Printf.sprintf "%s.%s must be an in-range integer" object_name name
  )

let int_field object_name name fields =
  match field name fields with
  | Some (`Int value) -> Ok value
  | Some (`Intlit value) ->
    (match int_of_string_opt value with
     | Some value -> Ok value
     | None -> int_error object_name name)
  | _ -> int_error object_name name

let bool_field_default default name fields =
  match field name fields with
  | Some (`Bool value) -> value
  | _ -> default

let list_field ?(null_is_empty=false) name fields =
  match field name fields with
  | None -> Ok []
  | Some `Null when null_is_empty -> Ok []
  | Some (`List values) -> Ok values
  | Some _ ->
    Error (
      Printf.sprintf "Control API %s field must be an array" name
    )

let finish_lines ~empty_message lines =
  match lines with
  | [] -> Ok empty_message
  | lines -> Ok (lines |> List.rev |> String.concat "\n")

let format_lines ?(null_list_is_empty=false)
    ~field_name ~empty_message ~line_of_item fields =
  match list_field ~null_is_empty:null_list_is_empty field_name fields with
  | Error _ as error -> error
  | Ok items ->
    let rec loop index lines = function
      | [] -> finish_lines ~empty_message lines
      | item :: rest ->
        match line_of_item index item with
        | Error _ as error -> error
        | Ok line -> loop (index + 1) (line :: lines) rest
    in
    loop 0 [] items

let format_response ?(null_list_is_empty=false)
    ~field_name ~empty_message ~line_of_item = function
  | `Assoc fields ->
    (match field "error" fields with
     | Some (`String message) -> Error message
     | Some _ -> Error "Control API error field must be a string"
     | None ->
       format_lines ~null_list_is_empty
         ~field_name ~empty_message ~line_of_item fields)
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

(* [Control_api.handle_list_sessions] applies [Resource.single_line] to
   [project_name] before this formatter interpolates it into a Discord
   markdown bullet. *)
let session_line = function
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
    (* Python treats explicit null sessions as empty because list_sessions
       guards with [if not sessions]; list_projects intentionally does not. *)
    ~null_list_is_empty:true
    ~field_name:"sessions"
    ~empty_message:"No active sessions."
    ~line_of_item:(fun _index -> session_line)
    response
