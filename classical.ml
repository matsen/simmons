
let int_div x y = ( float_of_int x ) /. (float_of_int y)


let mkstatZ s = TreeAE.StatZ.eval (TreeAE.StatZ.of_string s)
let mkstatR s = TreeAE.StatR.eval (TreeAE.StatR.of_string s)
let mk s = TreeAE.StatR.of_string s

let n_leaves = mkstatZ "1 | _+ x[1]"

let n_cherriesZ = mkstatZ "0;1|_+x[1] + =(2,_+x[2]);_+ x[2]"
let n_cherries t = float_of_int (n_cherriesZ t)

let ic_raw = mkstatZ "0; 1 | _>(x[2]-y[2]) + _+ x[1]; _+ x[2]"
let ic t =
  let n = n_leaves t in
  int_div (2*(ic_raw t)) ((n-1)*(n-2))

let nbar_raw = mkstatZ "0; 1 | _+ x[1] + _+ x[2]; _+ x[2]"
let nbar t = int_div (nbar_raw t) (n_leaves t)

(* the following formulation of i2 is a little "tricky." We use the
fact that a cherry will have _-[0] equal to zero and the fact that we
have defined 0/0 = 0
*)
let i2_raw = mkstatR
  "0;1|_+x[1]+(_>(x[2]-y[2])/((_+x[2])- 2));_+x[2]"
let i2 t = (i2_raw t) /. (float_of_int ((n_leaves t) - 2) )

(* b1: target zero finds the pathlengths, target one sums their
inverses it has to be wierd like this to avoid the root.
*)
let b1 = mkstatR "0;0|_+x[1] + _+( (1- =(x[2],0))/x[2] );1+_>x[2]"


(* stats which don't follow the alg form: *)
let get_leaf_depths t =
  let rec aux d = function
    | Btree.Node (l,r) -> ( aux (d+1) l ) @ ( aux (d+1) r )
    | Btree.Leaf -> [d]
  in
  aux 0 t

let varN t =
  let avg v =
    (List.fold_left (+.) 0. v) /. (float_of_int (List.length v)) in
  let var v =
    let a = avg v in
    avg (List.map (fun x -> (x -. a)*.(x -. a)) v)
  in
  var ( List.map float_of_int (get_leaf_depths t) )

let b2 t =
  List.fold_left
    (fun st ld -> st +. ( ld /. (2. ** ld) ) )
    0.
    (List.map float_of_int (get_leaf_depths t))


(* derived stats *)

let is_leaf = function
  | Btree.Node(l,r) -> false
  | Btree.Leaf -> true

let is_cherry_or_leaf = function
  | Btree.Node(l,r) -> (is_leaf l) && (is_leaf r)
  | Btree.Leaf -> true

(* apply f to all of the subtrees which are not cherries *)
let nc_subtree_map f t =
  let rec aux t =
    match t with
      | Btree.Node(l,r) ->
	  if (is_leaf l) && (is_leaf r) then
	    [] (* it's a cherry *)
	  else
	    (f t)::((aux l) @ (aux r))
      | Btree.Leaf -> []
  in
  aux t

(* note the negative sign so that big trees are on the left
*)
let fnl t = -. float_of_int (n_leaves t)

