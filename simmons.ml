
let run_together = ref false
let print_treenames = ref true
let calc_classical = ref false
let stat_fname = ref ""

let stats = ref []

let parse_args () =
  let files  = ref [] in
  let r = "-r", Arg.Set run_together,
    "Treat all tree files as one large file."
  and n = "-n", Arg.Clear print_treenames,
  "Suppress printing of tree names."
  and c = "-c", Arg.Set calc_classical,
  "Calculate the classical statistics and their first and second derived statistics."
  and f = "-f", Arg.Set_string stat_fname,
  "Specify a file of BRTSS statistics."
  and usage =
    "\nsimmons v0.1\nUsage: simmons [options] [file1 file2 ...]
    computes tree shape statistics. simmons accepts
  input from stdin, as in \'cat file1 | simmons\', or
  directly from files.\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [r; n; c; f] in
  Arg.parse args anon_arg usage;
  List.rev !files

let print_list fmt l =
  List.iter (fun x -> Printf.printf fmt x) l;
  Printf.printf "\n"

let write_stats tl =
  let stat_funs = List.map snd !stats in
  List.iter (
    fun (name,t) ->
      if !print_treenames then
        Printf.printf "%s\t" name;
      print_list "%g\t" (List.map (fun f -> f t) stat_funs)
  ) tl;
  ()



let process_file file name =
  let ret_code = ref 1
  and stat_names = List.map fst !stats
  and print_file_name () =
    match name with Some name -> Printf.printf "\n%s:\n" name | None -> ()
  in

  try
    if not !run_together then (
      print_file_name ();
      if !print_treenames then
        print_string "#name\t";
      print_list "%s\t" stat_names;
    );
    write_stats (Btree.tree_list_of_newick_channel file);
    !ret_code
  with Exit -> 0

let _ =
  let files = parse_args () in

  let basic_stats =
    [
      "I_c", Classical.ic;
      "N_bar", Classical.nbar;
      "I_2", Classical.i2;
      "B_1", Classical.b1;
      "B_2", Classical.b2;
      "Var_N", Classical.varN;
      "cherries",  Classical.n_cherries;
    ] in

  if !calc_classical then
    stats := basic_stats;

  if !stat_fname <> "" then (
    stats := !stats @ (TreeAE.StatR.fun_list_of_file !stat_fname);
  );

  let stat_names = List.map fst !stats in

  if files = [] then exit (process_file stdin None);
  if List.length files = 1 then run_together := true;

  if !run_together then (
    if !print_treenames then
      print_string "#name\t";
    print_list "%s\t" stat_names;
  );

  let collect ret_code filename =
    try
      let file = open_in filename in
      let frc = process_file file (Some filename) in
      close_in file;
      if frc = 0 && ret_code = 1 then 0 else ret_code
    with Sys_error msg -> prerr_endline msg; 2 in
  exit (List.fold_left collect 1 files)
