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
                    and type 'a stream = 'a Schema.Io.Stream.t) : sig

  type 'conn callback =
    'conn ->
    Cohttp.Request.t ->
    Body.t ->
    (Cohttp.Response.t * Body.t) Schema.Io.t

  val execute_request :
    'ctx Schema.schema ->
    'ctx ->
    Cohttp.Request.t ->
    Body.t ->
    (Cohttp.Response.t * Body.t) Schema.Io.t

  val make_callback :
    (Cohttp.Request.t -> 'ctx) ->
    'ctx Schema.schema ->
    'conn callback
end
