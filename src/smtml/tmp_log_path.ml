
let log_path : out_channel option ref = ref None
let solver_name : string ref = ref ""

let mutex = Mutex.create ()

let init path name =
  log_path := Some (Out_channel.open_gen [ Open_creat; Open_append; Open_text ] 0o644 path);
  solver_name := name

let write assumptions user_time system_time =
  (* 1. Sérialiser les données OCaml directement en bytes *)
  let data = (assumptions, !solver_name, user_time, system_time) in
  let bytes = Marshal.to_bytes data [] in

  (* 2. Écrire les bytes dans le flux gzip, avec mutex pour la concurrence *)
  match !log_path with
  | None -> Fmt.failwith "log not initialized"
  | Some oc ->
    Mutex.lock mutex;
    Out_channel.output_bytes oc bytes;
    Mutex.unlock mutex

let close () =
  match !log_path with
  | None -> ()
  | Some oc ->
    Out_channel.close oc;
    log_path := None
