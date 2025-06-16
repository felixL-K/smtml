(* tmp_log_path.ml *)
let log_out : Gzip.out_channel option ref = ref None

let mutex : Mutex.t = Mutex.create ()

let init path = log_out := Some (Gzip.open_out ~level:9 path)

let write_line s =
  match !log_out with
  | None -> Fmt.failwith "log not initialized"
  | Some oc ->
    let buffer = String.to_bytes s in
    let len = Bytes.length buffer in
    Mutex.lock mutex;
    Gzip.output oc buffer 0 len;
    Mutex.unlock mutex

let close () =
  match !log_out with
  | None -> ()
  | Some oc ->
    Gzip.close_out oc;
    log_out := None
