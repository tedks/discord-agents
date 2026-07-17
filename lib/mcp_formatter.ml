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

let bool_field_default default name fields =
  match field name fields with
  | Some (`Bool value) -> value
  | _ -> default

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

let format_list_projects = function
  | `Assoc fields ->
    (match field "error" fields with
     | Some (`String message) -> Error message
     | Some _ -> Error "Control API error field must be a string"
     | None ->
       let projects =
         match field "projects" fields with
         | None -> Ok []
         | Some (`List projects) -> Ok projects
         | Some _ -> Error "Control API projects field must be an array"
       in
       (match projects with
        | Error _ as error -> error
        | Ok [] -> Ok "No projects found."
        | Ok projects ->
          projects
          |> List.mapi project_line
          |> List.fold_left (fun acc line ->
            match acc, line with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok lines, Ok line -> Ok (line :: lines))
            (Ok [])
          |> Result.map (fun lines ->
            lines |> List.rev |> String.concat "\n")))
  | _ -> Error "Control API response must be an object"
