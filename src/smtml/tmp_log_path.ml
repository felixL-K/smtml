
let log_path : out_channel option ref = ref None

let mutex = Mutex.create ()

let init path =
  log_path := Some (Out_channel.open_gen [ Open_creat; Open_append; Open_text ] 0o644 (Fpath.to_string path))

let write assumptions user_time system_time =
  let data = (assumptions, user_time, system_time) in
  let bytes = Marshal.to_bytes data [] in

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
