module type HttpBody = sig
  type t
  type +'a io
  type 'a stream

  val to_string : t -> string io
  val of_string : string -> t
  val of_stream : string stream -> t
end

module Make
  (Schema : Graphql_intf.Schema)
  (Body : HttpBody with type +'a io = 'a Schema.Io.t
                    and type 'a stream = 'a Schema.Io.Stream.t) = struct

  module Io = Schema.Io

  let (>>=) = Io.bind

  type 'conn callback =
    'conn ->
    Cohttp.Request.t ->
    Body.t ->
    (Cohttp.Response.t * Body.t) Schema.Io.t

  let respond_string ~status ~body () =
    Io.return (Cohttp.Response.make ~status (), Body.of_string body)

  let static_file_response path =
    match Assets.read path with
    | Some body -> respond_string ~status:`OK ~body ()
    | None -> respond_string ~status:`Not_found ~body:"" ()

  let json_err = function
    | Ok _ as ok -> ok
    | Error err -> Error (`String err)

  let execute_query ctx schema variables operation_name query =
    let parser_result = json_err (Graphql_parser.parse query) in
    Io.return parser_result >>= function
    | Ok doc -> Schema.execute schema ctx ~variables ?operation_name doc
    | Error _ as e -> Io.return e

  let execute_request schema ctx _req body =
    Body.to_string body >>= fun body' ->
    let json = Yojson.Basic.from_string body' in
    let query = Yojson.Basic.(json |> Util.member "query" |> Util.to_string) in
    let variables = try Yojson.Basic.Util.(json |> member "variables" |> to_assoc) with _ -> [] in
    let operation_name =
      try Some Yojson.Basic.Util.(json |> member "operationName" |> to_string)
      with _ -> None
    in
    let result = execute_query ctx schema (variables :> (string * Graphql_parser.const_value) list) operation_name query in
    result >>= function
    | Ok (`Response data) ->
      let body = Yojson.Basic.to_string data in
      respond_string ~status:`OK ~body ()
    | Ok (`Stream stream) ->
      let body_stream =
        Io.Stream.map stream (function
        | Ok json
        | Error json ->
            Io.return (Format.sprintf "data: %s\n\n" (Yojson.Basic.to_string json))
      ) in
      let headers = Cohttp.Header.of_list ["Content-Type", "text/event-stream"] in
      let response = Cohttp.Response.make ~headers () in
      let body = Body.of_stream body_stream in
      Io.return (response, body)
    | Error err ->
      let body = Yojson.Basic.to_string err in
      respond_string ~status:`Internal_server_error ~body ()

  let make_callback make_context schema _conn (req : Cohttp.Request.t) body =
    let req_path = Cohttp.Request.uri req |> Uri.path in
    let path_parts = Str.(split (regexp "/") req_path) in
    match req.meth, path_parts with
    | `GET,  ["graphql"]       -> static_file_response "index.html"
    | `GET,  ["graphql"; path] -> static_file_response path
    | `POST, ["graphql"]       -> execute_request schema (make_context req) req body
    | _ -> respond_string ~status:`Not_found ~body:"" ()
end
