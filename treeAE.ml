



module Stat (N: Number.NUMBER) =
struct
  type s = N.t
  type ae = Algexp.Tvs(N).t
  type t = Stat of s list * ae list
  module TvsN = Algexp.Tvs(N) 
(* basics *)
  let to_string_real = function
    Stat(sl, ael) ->
      String.concat "|" 
	[
	  ( String.concat ";" ( List.map N.to_string sl )) ;
	  ( String.concat ";" ( List.map TvsN.to_string ael ))
	]
  let to_string_safe = function
    Stat(sl, ael) ->
      String.concat "|" 
	[
	  ( String.concat ";" ( List.map N.to_string sl )) ;
	  ( String.concat ";" ( List.map TvsN.to_string_safe ael ))
	]
  let check_indices ael = 
    List.iter (
      fun ae -> 
	match TvsN.extremes_of_var_indices ae with
	  | Some (min,max) ->
	      if min < 1 || max > (List.length ael) then
		failwith ("Index out of bounds: "^(TvsN.to_string ae));
	  | None -> ()
    ) ael
  let create sl ael = 
    assert ( List.length sl = List.length ael );
    check_indices ael;
    Stat(sl, ael)

  let get_s = function Stat(sl, ael) -> sl
  let get_ae = function Stat(sl, ael) -> ael
  let eval = function 
    Stat(sl, ael) -> 
      let funs = List.map
	(fun ae -> (fun ingo -> TvsN.eval ae ingo false)) ael in
      fun t -> List.hd (Btree.app_rec funs sl t)
(* parsing *)
  let of_string str = 
    let split_on p s = Pcre.split ~pat:p s in
    let clean_space s = Pcre.replace ~pat:"\\s" ~templ:"" s in
    let parts = split_on "\\|" str in
    if (List.length parts) <> 2 then
      failwith ("Exactly one | per stat: "^str);
    let ael = 
      List.map (TvsN.of_string) (
	split_on ";" (
	  List.nth parts 1
	)
      )
    in
    let sl = 
      List.map (fun s -> N.of_string (clean_space s)) (
	split_on ";" (
	  List.nth parts 0
	)
      )
    in
    if( (List.length ael) <> (List.length sl) ) then 
      failwith ("Two lists not same length: "^str);
    create sl ael

  let rex = Pcre.regexp "\\S+\\|\\S+"
    
  let stat_list_of_channel cin = 
    Common_base.channel_to_thing_list 
      cin
      (Pcre.pmatch ~rex:rex)
      (* make a string into an actual statistic *)
      (fun statstr -> 
	 let ss = (Pcre.extract ~rex:rex statstr).(0) in
	 (* above: assume only one stat per line, or take the first *)
         (ss, of_string ss))

  let fun_list_of_channel cin = 
    List.map (
	fun (name, stat) -> 
          (name, fun t -> eval stat t)
      ) (stat_list_of_channel cin)

  let transform_ch_to_fname of_fn fname = 
    let cin = open_in fname in
    let p = of_fn cin in
    close_in cin;
    p

  let stat_list_of_file = transform_ch_to_fname stat_list_of_channel
  let fun_list_of_file = transform_ch_to_fname fun_list_of_channel

  let extend_neg = function
      Stat(sl, ael) ->
	Stat(sl, List.map TvsN.extend_neg ael)

  let to_string s = 
    let sx = extend_neg s in
    let str = to_string_real sx in
    if sx <> of_string str then
	print_string ( "TreeAE's of_string failed to parse "
		   ^ (to_string_safe sx) ^ "\n" );
    str
end
  
module StatZ = Stat(Number.Z)
module StatR = Stat(Number.R)


(*
let read s = TreeAE.StatR.of_string s
let q1 = read "1|_>(y[1])"
let q2 = read "1|-(_*x[1])"

let read s = TreeAE.StatR.of_string s
let test s = TreeAE.StatR.to_string (TreeAE.StatR.of_string s)
*)

(*
mystery error:


Fatal error: exception Failure("TreeAE's of_string failed to parse
1|_>-(y[1]*((((>(_>((y[1]-1)/>(4,p(p(x[1],2),3)*2)),a(>(-1,a(x[1]))/x[1]))+(
x[1]+x[1]))-2)/9)+x[1]))
")


however...


# let q = test "1|_>-(y[1]*((((>(_>((y[1]-1)/>(4,p(p(x[1],2),3)*2)),a(>(-1,a(x[1]))/x[1]))+(
x[1]+x[1]))-2)/9)+x[1]))";;
  val q : string =
  "1|_>-(y[1]*((((>(_>((y[1]-1)/>(4,p(p(x[1],2),3)*2)),a(>(-1,a(x[1]))/x[1]))+(x[1]+x[1]))-2)/9)+x[1]))"

works fine... 
*)

(*
another:



let q = test "1|_>((0.333333/v(y[1]))*-(_>y[1]+x[1]))"

(*
let q = test "1|_>((0.333333/v(y[1]))*-(_>y[1]+x[1]))";;
val q : string = "1|_>(0.333333/v(y[1])*-(_>y[1]+x[1]))"
*)

let q1 = read "1|_>((3/v(y[1]))*-(_>y[1]+x[1]))"
let q2 = read "1|_>(3/v(y[1])*-(_>y[1]+x[1]))"


let test s = TreeAE.StatR.to_string (TreeAE.StatR.of_string s)

let q = test "4|-(4*_+x[1])";;



*)
