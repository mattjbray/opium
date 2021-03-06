module Export = struct
  include Opium_kernel.Export
  module App = Opium_app
end

include Export

module App_export = struct
  (* selectively export the most useful parts of App *)
  let param     = App.param
  let splat     = App.splat
  let respond   = App.respond
  let respond'  = App.respond'
  let redirect  = App.redirect
  let redirect' = App.redirect'

  let get    = App.get
  let post   = App.post
  let put    = App.put
  let delete = App.delete

  let all = App.all
  let any = App.any

  let middleware = App.middleware
end

module Middleware = struct
  (** Re-exports simple middleware that doesn't have auxiliary
      functions *)
  let debug = Opium_debug.debug
  let trace = Opium_debug.trace
  let static = Opium_static_serve.m
end

module Std = struct
  include Export
  module Middleware = Middleware
  include App_export
  module Body = Cohttp_lwt_body
end
