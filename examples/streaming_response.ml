open Lwt.Infix
open Opium.Std

let hello =
  get "/" (fun req ->
    Lwt_log.notice "Request received." >>= fun () ->

    Lwt_unix.sleep 1.0 >>= fun () ->

    Lwt_log.notice "Starting tasks" >>= fun () ->

    let stream, push = Lwt_stream.create () in

    (* Parallel iterate over a list, pushing results onto the stream. *)
    let worker =
      [ 3.0 ; 1.0 ; 2.0; 5.0; 4.0 ]
      |> Lwt_list.iteri_p
           (fun i seconds ->
              Lwt_unix.sleep seconds >>= fun () ->
              push (Some (string_of_int i ^ "\n"));
              Lwt_log.notice_f "Task %i complete." i)
      >>= fun () ->
      Lwt_log.notice "All tasks completed without interruption."
    in

    (* When all worker threads are done, close the stream. *)
    let () =
      Lwt.ignore_result
        (Lwt.finalize
           (fun () -> worker)
           (fun () ->
              push None;
              Lwt_log.notice_f "Closed stream."))
    in

    let body = Cohttp_lwt_body.of_stream stream in

    (* Cancel the worker threads if the client goes away. *)
    let conn_closed () =
      Lwt_log.notice "Cancelling worker." >>= fun () ->
      Lwt.cancel worker |> Lwt.return
    in

    Opium.Response.create ~body ~code:`OK ~conn_closed ()
    |> Lwt.return
  )

let () =
  Lwt.async_exception_hook :=
    (fun exn ->
       Lwt_log.ign_error_f "Uncaught exception: %s" (Printexc.to_string exn))

let () = App.empty
         |> hello
         |> App.run_command
         |> ignore
