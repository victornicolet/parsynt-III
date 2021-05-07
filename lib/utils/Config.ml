open Base

(* General parameters *)

let skip_predicate_discovery = ref false

let skip_first_solve = ref false

let break_problem = ref true

let always_use_lwt = ref true

let default_lift = ref 0

let minic_input = ref true

let parse_minic_only = ref false

let master_file = ref "undef"

(* Other verbosity flags *)
let complexity_verbose = ref 0

(* Symbolic execution parameters *)

let symbexe_unfolding_len = ref 3

let sfsp_synt_len = ref 4

let sl_join_synt_len = ref 5

let sfsp_sketch_complex = ref 0

(* Bounds on synthesis main loop *)

let cmax = ref 3

let budget_not_specified = ref true

let dump_solutions = ref false

let k = ref (-1)

let set_k (s : string) =
  try
    let n = Int.of_string s in
    if n < 0 || n > 2 then (
      Log.error_msg "k should be be between 0 and 2 inclusive.";
      failwith "Please specify a valid value for k." )
    else k := n
  with _ -> Log.warning_msg "Failed to parse value of k. Ignoring provided value."

let m = ref (-1)

let set_m (s : string) =
  try
    let n = Int.of_string s in
    if n < 2 then (
      Log.error_msg "m should be >= 2..";
      failwith "Please specify a valid value for m." )
    else m := n
  with _ -> Log.warning_msg "Failed to parse value of m. Ignoring provided value."

let c = ref (-1)

let set_c (s : string) =
  try
    let n = Int.of_string s in
    if n < 2 then (
      Log.error_msg "c should be >= 2..";
      failwith "Please specify a valid value for c." )
    else c := n
  with _ -> Log.warning_msg "Failed to parse value of c. Ignoring provided value."

(* Performance parameters *)
let num_parallel_processes = 4

let timeout_sfsp_synt = 100

(* Programs : TODO *)
let racket = FileUtil.which "racket"

let z3 = FileUtil.which "z3"

let tmp_dir = Caml.Filename.get_temp_dir_name ()
