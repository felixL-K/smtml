

let log_path : string option ref = ref None

let set path = log_path := Some path

let get () =
  match !log_path with
  | Some p -> p
  | None ->
    let env_var = "QUERY_LOG_PATH" in
    match Bos.OS.Env.var env_var with
    | Some path ->
        log_path := Some path;
        path
    | None ->
        Fmt.failwith "Temporary log path not set and QUERY_LOG_PATH is not defined"

let init_logging () =
  let env_var = "QUERY_LOG_PATH" in
  match Bos.OS.Env.var env_var with
  | Some path ->
      set path
  | None ->
      let pid = Unix.getpid () in
      let unique_name = Fmt.str "queries_log_%d.jsonl" pid in
      let tmp_path = String.concat "" ["/home/intern-fw-03/Documents/queries"; unique_name] in
      set tmp_path
