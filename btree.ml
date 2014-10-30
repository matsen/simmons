(*pp camlp4o -I .  *)

(*
#load "camlp4o.cma";
#load "pa_extend.cmo"
*)

type btree = Node of btree * btree
	     | Leaf

let join t s = Node(t,s)

let rec comb n = 
  if n = 1 then Leaf
  else join (comb (n-1)) Leaf

let rec balanced n = 
  let round_up x = int_of_float (ceil x) in
  let round_down x = int_of_float (floor x) in
  if n = 1 then Leaf
  else
    let half = (float_of_int n) /. 2. in
    join
      (balanced (round_up half)) 
      (balanced (round_down half))

let rec n_leaves = function
  | Node(l,r) -> (n_leaves l) + (n_leaves r)
  | Leaf -> 1

let rec depth = function
  | Node(l,r) -> 1 + max ( depth l) ( depth r )
  | Leaf -> 0

let to_newick t = 
  let rec aux = function
    | Node(l,r) -> Printf.sprintf "(%s,%s)" (aux l) (aux r)
    | Leaf -> Printf.sprintf "x"
  in
  (aux t)^";"

let int_to_string_of_width x desired_width = 
  let s = string_of_int x in
  let width = String.length s in
  if width > desired_width then
    failwith "String already too wide in int_to_string_of_width";
  let rec add_zeroes str n = 
    if n > 0 then add_zeroes ("0"^str) (n-1)
    else str 
  in
  add_zeroes s (desired_width - width)

let to_newick_numbered t = 
  let n = n_leaves t in
  let width = 1 + int_of_float ( log10 ( float_of_int n ) ) in
  let count = ref (n+1) in
  let rec aux = function
    | Node(l,r) -> Printf.sprintf "(%s,%s)" (aux l) (aux r)
    | Leaf -> 
	decr count; 
	"x"^( int_to_string_of_width !count width )
  in
  (aux t)^";"

let print_newick t = 
  Printf.printf "%s\n" (to_newick t)

let of_newick str = 
  let lexer = Genlex.make_lexer ["(";")";","] in
  (* check_tree just checks to see if the tree is OK, even if not
     bifurcating *)
  let check_tree str = 
    (* the first two functions set up the basic parsing "loop" to
       parse a comma-separated list *)
    let rec parse_expr = parser
      | [< _ = parse_st ; _ = parse_remainder >] -> ()
    and parse_remainder = parser
      | [< _ = parse_expr >] -> ()
      | [< >] -> ()
	  (* this parser actually parses a leaf or a subtree.*)
    and parse_st = parser
      | [< 'Genlex.Ident x ; _ = terminate_st >] -> ()
      | [< 'Genlex.Kwd "("; _ = parse_expr; 'Genlex.Kwd ")"; 
	   _ = terminate_st >] -> () 
    and terminate_st = parser
      | [< 'Genlex.Kwd "," >] -> ()
      | [< >] -> ()
    in
    try 
      match lexer (Stream.of_string str) with parser
          [< t = parse_expr; _ = Stream.empty >] -> t
    with
      | Stream.Error s -> 
          failwith ("Btree.of_newick: your tree appears to be badly formed. Please check parens: "^str)    
      | Stream.Failure -> 
          failwith ("Btree.of_newick: your tree appears to contain strange characters: "^str)    
  in
  (* parse_tree assumes the tree is bifurcating *)
  let rec parse_tree = parser
    | [< 'Genlex.Ident x  >] -> Leaf
    | [< 'Genlex.Kwd "("; t1 = parse_tree; 
	 'Genlex.Kwd ","; t2 = parse_tree; 'Genlex.Kwd ")" >] -> 
	join t1 t2 
  in
  (* we first remove extra space, then we delete edgelengths, then we
     add fillers when there is no name or edgelength, then we remove
     the trailing semicolon if it exists, then we replace all the
     names by x's. because of the matching dynamics of pcre, we have
    Pcre.replace ~pat:"[_a-zA-Z]+(\\.[_a-zA-Z])*" ~templ:"x" (
     to run the ",," substitution twice. *)
  let edited = 
    Pcre.replace ~pat:"[^,;()'\\s]+(\\.[^,;()'\\s])*" ~templ:"x" (
      Pcre.replace ~pat:";$" ~templ:"" (
	Pcre.replace ~pat:"\\(," ~templ:"(x," (
          Pcre.replace ~pat:",\\)" ~templ:",x)" (
	    Pcre.replace ~pat:",," ~templ:",x," (
	      Pcre.replace ~pat:",," ~templ:",x," (
		Pcre.replace 
		  ~pat:
		  ":([-+]?(?:[0-9]*\\.)?[0-9]+(?:[eE][-+]?[0-9]+)?)" 
		  ~templ:"" (
		    Pcre.replace ~pat:"\\s" ~templ:"" (
		      str
		    ))))))))
  in
  (*print_string (edited^"\n");*)
  check_tree edited;
  try 
    match lexer (Stream.of_string edited) with parser
	[< t = parse_tree; _ = Stream.empty >] -> t
  with
      Stream.Error s -> 
	failwith ("Btree.of_newick: subroutine only accepts bifurcating trees. The offending tree is: "^str)


let tree_str_list_of_newick_channel cin = 
  let trees = ref [] in
  (* we collect lines of a tree in tree_strs *)
  let tree_strs = ref [] in
  let name = ref "" in
  let add_tree () = 
    if !tree_strs <> [] then (
      let combined =
        Pcre.replace ~pat:"\\s" ~templ:"" (
          String.concat "" (List.rev !tree_strs)) in
      trees := 
        ((if !name <> "" then !name else combined^";"), combined)::!trees;
      tree_strs := [];
      name := "";
    )
  in
  try 
    let line = ref (input_line cin) in
    while true do
      if Pcre.pmatch ~pat:";" !line then (
	(* we may have several trees on the same line *)
	(* below: setting max to -1 does not strip empty fields, which
	   is good because if a line does terminate with a semicolon,
	   then ??? *)
	let strs = Pcre.split ~max:(-1) ~pat:";" !line in
        tree_strs := ((List.hd strs)::!tree_strs);
	add_tree ();
	line := String.concat ";" (List.tl strs);
      )
      else (
	(* we load a new line at the end *)
	if Pcre.pmatch ~pat:"^\\s*(#.*)?$" !line then 
	  (* if comment or empty line *)
          ()
	else if !tree_strs = [] && Pcre.pmatch ~pat:"^>" !line then 
          (* if the first line begins with a caret then it's a name *)
          name := Pcre.replace ~pat:"^>" !line
	else
          (* keep adding strings *)
          tree_strs := (!line::!tree_strs);
	line := input_line cin;
      )
    done;
    List.rev !trees
  with
      End_of_file -> 
        add_tree ();
	(*print_string "the trees:\n";
	List.iter (fun (name,t) -> Printf.printf "%s\n" (to_newick t)) 
	  (List.rev !trees);*)
	flush stdout;
        List.rev !trees (* adding with :: reverses order *)

let tree_list_of_newick_channel cin = 
  List.map (
    fun (name,tree_str) ->
      (name, of_newick tree_str) )
  (tree_str_list_of_newick_channel cin)

let file_fun_of_ch_fun f fname = 
  let ch = open_in fname in
  let result = f ch in
  close_in ch;
  result

let tree_str_list_of_newick_file = 
  file_fun_of_ch_fun tree_str_list_of_newick_channel

let tree_list_of_newick_file = 
  file_fun_of_ch_fun tree_list_of_newick_channel

let iter_over_all_up_to max_leaves f = 
  let trees = Array.create (max_leaves+1) (Array.create 1 Leaf) in
  (* note that the trees array is actually indexed by the number of
     leaves, not one less than it! Therefore split goes from 1 to 
     n_leaves/2. *)
  for n_leaves=2 to max_leaves do
    let newtrees = ref [] in
    for split=1 to n_leaves/2 do
      let left = trees.(n_leaves-split) in
      let right = trees.(split) in
      for i=0 to Array.length left - 1 do
	(* this start thing is because we need to combine all subtrees
	   of size n/2, and be careful to avoid duplicates *)
	let start = 
	  if ((n_leaves mod 2) = 0) && ((n_leaves/2) = split) then i
	  else 0
	in
        for j=start to (Array.length right) - 1 do
          newtrees := (join left.(i) right.(j))::!newtrees;
        done;
      done;
    done;
    trees.(n_leaves) <- Array.of_list (List.rev !newtrees);
    f n_leaves trees.(n_leaves);
  done;
  ()

let generate_all n_leaves = 
  let some_trees = ref (Array.create 1 Leaf) in
  iter_over_all_up_to n_leaves (
    fun n_l t_arr ->
      if n_l = n_leaves then some_trees := t_arr
  );
  !some_trees

let print_all_trees ch n_leaves = 
  Printf.fprintf ch "%s"
    ("#all trees of size "^(string_of_int n_leaves)^":\n");
  Array.iter 
    (fun t -> Printf.fprintf ch "%s\n" (to_newick t))
    (generate_all n_leaves);
  print_string "\n";
  ()
    
let all_trees_to_file n_leaves fname = 
  let ch = open_out fname in
  print_all_trees ch n_leaves;
  close_out ch;
  ()

let app_rec fun_list leaf_val_list t =
  let rec aux = function
    | Node(l,r) -> 
        let ingo = (aux l, aux r) in
        List.map ( fun f -> f ingo ) fun_list
    | Leaf -> leaf_val_list
  in
  aux t
