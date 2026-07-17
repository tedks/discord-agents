(** Minimal MCP JSON-RPC protocol handling.

    The Python MCP server remains the runtime entrypoint for tool calls while
    formatting is ported. This module owns the protocol surface that can be
    tested against the typed OCaml tool descriptors. *)

type tool_call = {
  name : string;
  arguments : Yojson.Safe.t;
}

type tool_handler = tool_call -> (string, string) result

let protocol_version = "2024-11-05"

let server_name = "discord-agents-mcp"

let server_version = "0.3.0"

let raise_if_fatal exn =
  match exn with
  | Out_of_memory
  | Stack_overflow
  | Sys.Break -> raise exn
  | _ -> ()

let field name fields =
  List.assoc_opt name fields

let string_field name fields =
  match field name fields with
  | Some (`String value) -> value
  | _ -> ""

let id_field fields =
  match field "id" fields with
  | Some id -> id
  | None -> `Null

let response ~id ~result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result);
  ]

let error ~id ~code ~message =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
    ]);
  ]

let initialize_result =
  `Assoc [
    ("protocolVersion", `String protocol_version);
    ("capabilities", `Assoc [("tools", `Assoc [])]);
    ("serverInfo", `Assoc [
      ("name", `String server_name);
      ("version", `String server_version);
    ]);
  ]

let text_content text =
  `List [
    `Assoc [
      ("type", `String "text");
      ("text", `String text);
    ];
  ]

let tool_response ?(is_error=false) text =
  let fields = [("content", text_content text)] in
  `Assoc (
    if is_error then fields @ [("isError", `Bool true)] else fields
  )

let tools_list_result =
  `Assoc [("tools", Mcp_tool.tool_definitions_json)]

let call_tool handle_tool_call fields =
  let params =
    match field "params" fields with
    | Some (`Assoc params) -> params
    | _ -> []
  in
  let arguments =
    match field "arguments" params with
    | Some arguments -> arguments
    | None -> `Assoc []
  in
  let call = {
    name = string_field "name" params;
    arguments;
  } in
  try
    match handle_tool_call call with
    | Ok text -> tool_response text
    | Error message -> tool_response ~is_error:true message
  with exn ->
    raise_if_fatal exn;
    tool_response ~is_error:true (Printf.sprintf "Error: %s" (Printexc.to_string exn))

let handle_json ~handle_tool_call = function
  | `Assoc fields ->
    let id = id_field fields in
    (match string_field "method" fields with
     | "initialize" -> Some (response ~id ~result:initialize_result)
     | "notifications/initialized" -> None
     | "tools/list" -> Some (response ~id ~result:tools_list_result)
     | "tools/call" ->
       Some (response ~id ~result:(call_tool handle_tool_call fields))
     | "ping" -> Some (response ~id ~result:(`Assoc []))
     | method_name ->
       if field "id" fields = None then None
       else
         Some (error ~id ~code:(-32601)
                 ~message:(Printf.sprintf "Unknown method: %s" method_name)))
  | _ -> None

let handle_line ~handle_tool_call line =
  match String.trim line with
  | "" -> None
  | trimmed ->
    match Yojson.Safe.from_string trimmed with
    | json -> handle_json ~handle_tool_call json
    | exception Yojson.Json_error _ -> None
