

let log_path : string option ref = ref None

let set path = log_path := Some path

let get () =
  match !log_path with
  | Some p -> p
  | None -> Fmt.failwith "Temporary log path not set"

let init_logging () =
  let pid = Unix.getpid () in
  let unique_name = Fmt.str "queries_log_%d.jsonl" pid in
  let tmp_path = String.concat "" ["/home/intern-fw-03/Documents/queries/"; unique_name] in
  set tmp_path
