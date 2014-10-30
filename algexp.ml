(*pp camlp4o -I .  *)


(*
#load "camlp4o.cma";
#load "pa_extend.cma"
*)

let xor a b = ((not a) || (not b)) && (a || b)

module Tvs (N: Number.NUMBER) =
struct
  (* scalar type *)
  type s = N.t
  type sb_op = SBSum | SBProd | SBMax | SBIndic
  type t = | Sum of t * t
	   | Diff of t * t
	   | Prod of t * t
	   | Quot of t * t
	   | Pow of t * t
	   | Max of t * t
	   | Indic of t * t
	   | Abs of t
	   | Neg of t
	   | Inv of t
	   | Exp of t
	   | Log of t
	   | Const of s
	   | Var of bool * int
	   | Symm of sb_op * t
  let of_int x = Const(N.of_int x)
  let of_float x = Const(N.of_float x)
  let sum a b = Sum (a, b)
  let diff a b = Diff (a, b)
  let prod a b = Prod (a, b)
  let quot a b = Quot (a, b)
  let pow a b = Pow (a, b)
  let max a b = Max (a, b)
  let indic a b = Indic (a, b)
  let abs a = Abs a
  let neg a = Neg a
  let inv a = Inv a
  let exp a = Exp a
  let log a = Log a
  let const x = Const x
  let var b i = Var(b, i)
  let symm op a = Symm(op,a)
    (* flip determines wether or not to reverse the x's and y's *)
  let eval expr arg flip = 
    let rec aux fl = function
      | Sum (a, b) -> N.add (aux fl a) (aux fl b)
      | Diff (a, b) -> N.sub (aux fl a) (aux fl b)
      | Prod (a, b) -> N.mul (aux fl a) (aux fl b)
      | Quot (a, b) -> N.div (aux fl a) (aux fl b)
      | Pow (a, b) -> N.pow (aux fl a) (aux fl b)
      | Max (a, b) -> N.max (aux fl a) (aux fl b)
      | Indic (a, b) -> N.indic (aux fl a) (aux fl b)
      | Neg (a) -> N.neg (aux fl a)
      | Abs (a) -> N.abs (aux fl a)
      | Inv (a) -> N.inv (aux fl a)
      | Exp (a) -> N.exp (aux fl a)
      | Log (a) -> N.log (aux fl a)
      | Const (x) -> x
      | Var (b, i) -> 
	  (* if we flip or if b = true (i.e. it's y) then snd *)
	  List.nth (if xor b fl then snd arg else fst arg) (i-1)
      | Symm (op, a) -> 
	  match op with
	    | SBSum -> N.add (aux false a) (aux true a)
	    | SBProd -> N.mul (aux false a) (aux true a)
	    | SBMax -> N.max (aux false a) (aux true a)
	    | SBIndic -> N.indic (aux false a) (aux true a)
      in
    aux flip expr
  let rec is_symm = function
      | Sum (a, b) -> (is_symm a) && (is_symm b)
      | Diff (a, b) -> (is_symm a) && (is_symm b)
      | Prod (a, b) -> (is_symm a) && (is_symm b)
      | Quot (a, b) -> (is_symm a) && (is_symm b)
      | Pow (a, b) -> (is_symm a) && (is_symm b)
      | Max (a, b) -> (is_symm a) && (is_symm b)
      | Indic (a, b) -> (is_symm a) && (is_symm b)
      | Neg (a) -> is_symm a
      | Abs (a) -> is_symm a
      | Inv (a) -> is_symm a
      | Exp (a) -> is_symm a
      | Log (a) -> is_symm a
      | Const (x) -> true
      | Var (b, i) -> false
      | Symm (op, a) -> true
(* flip changes the x_i's to y_i's and vv *)
  let rec flip = function
      | Sum (a, b) -> Sum(flip a, flip b)
      | Diff (a, b) -> Diff(flip a, flip b)
      | Prod (a, b) -> Prod(flip a, flip b)
      | Quot (a, b) -> Quot(flip a, flip b)
      | Pow (a, b) -> Pow(flip a, flip b)
      | Max (a, b) -> Max(flip a, flip b)
      | Indic (a, b) -> Indic(flip a, flip b)
      | Neg (a) -> Neg(flip a)
      | Abs (a) -> Abs(flip a)
      | Inv (a) -> Inv(flip a)
      | Exp (a) -> Exp(flip a)
      | Log (a) -> Log(flip a)
      | Const (x) -> Const(x)
      | Var (b, i) -> Var(not b, i)
      | Symm (op, a) -> Symm(op, flip a)
(* boost increases (or decr) the indices of the vars by amount*)
  let boost amount exp = 
    let rec aux = function
      | Sum (a, b) -> Sum(aux a, aux b)
      | Diff (a, b) -> Diff(aux a, aux b)
      | Prod (a, b) -> Prod(aux a, aux b)
      | Quot (a, b) -> Quot(aux a, aux b)
      | Pow (a, b) -> Pow(aux a, aux b)
      | Max (a, b) -> Max(aux a, aux b)
      | Indic (a, b) -> Indic(aux a, aux b)
      | Neg (a) -> Neg(aux a)
      | Abs (a) -> Abs(aux a)
      | Inv (a) -> Inv(aux a)
      | Exp (a) -> Exp(aux a)
      | Log (a) -> Log(aux a)
      | Const (x) -> Const(x)
      | Var (b, i) -> Var(b, i+amount)
      | Symm (op, a) -> Symm(op, aux a)
    in
    aux exp

  (* extend_neg makes negative numbers explicitly use a negation. this
     is necessary because of how we parse *)
  let rec extend_neg = function
    | Sum (a, b) -> Sum(extend_neg a, extend_neg b)
    | Diff (a, b) -> Diff(extend_neg a, extend_neg b)
    | Prod (a, b) -> Prod(extend_neg a, extend_neg b)
    | Quot (a, b) -> Quot(extend_neg a, extend_neg b)
    | Pow (a, b) -> Pow(extend_neg a, extend_neg b)
    | Max (a, b) -> Max(extend_neg a, extend_neg b)
    | Indic (a, b) -> Indic(extend_neg a, extend_neg b)
    | Neg (a) -> Neg(extend_neg a)
    | Abs (a) -> Abs(extend_neg a)
    | Inv (a) -> Inv(extend_neg a)
    | Exp (a) -> Exp(extend_neg a)
    | Log (a) -> Log(extend_neg a)
    | Const (x) -> 
	if N.abs x = x then Const(x)
	else Neg( Const( N.abs x ) )
    | Var (b, i) -> Var(b, i)
    | Symm (op, a) -> Symm(op, extend_neg a)
  let extremes_of_var_indices exp = 
    let bin_max_min = function
      | (Some (fmin,fmax),Some (gmin,gmax)) 
	-> Some (Pervasives.min fmin gmin, Pervasives.max fmax gmax)
      | (Some (fmin,fmax),None) 
	-> Some (fmin, fmax)
      | (None, Some (fmin,fmax)) 
	-> Some (fmin, fmax)
      | (None, None) -> None
    in
    let rec aux = function
      | Sum (f, g) -> bin_max_min (aux f, aux g)
      | Diff (f, g) -> bin_max_min (aux f, aux g)
      | Prod (f, g) -> bin_max_min (aux f, aux g)
      | Quot (f, g) -> bin_max_min (aux f, aux g)
      | Pow (f, g) -> bin_max_min (aux f, aux g)
      | Max (f, g) -> bin_max_min (aux f, aux g)
      | Indic (f, g) -> bin_max_min (aux f, aux g)
      | Neg (f) -> aux f
      | Abs (f) -> aux f
      | Inv (f) -> aux f
      | Exp (f) -> aux f
      | Log (f) -> aux f
      | Const (x) -> None
      | Var (b, i) -> Some (i,i)
      | Symm (op, a) -> aux a
    in
    aux exp

  (* note that this function does make some implicit simplifications
     based on associativity, passing the minus, etc. for example Neg(2)*3
     and Neg(2*3) parse the same. thus we call it unsafe *)
  let to_string_unsafe exp =
    let wrap_paren prec op_prec str =
      (if prec > op_prec then "(" else "")
      ^str
      ^(if prec > op_prec then ")" else "")
    in
    let rec aux prec exp =     (* prec is the current precedence *)
      match exp with
        | Sum(f, g) ->
            wrap_paren prec 0 ((aux 0 f)^"+"^(aux 0 g))
        | Diff (f, g) -> 
            wrap_paren prec 0 ((aux 0 f)^"-"^(aux 0 g))
        | Prod(f, g) ->
            wrap_paren prec 2 ((aux 2 f)^"*"^(aux 2 g))
        | Quot(f, g) ->
            wrap_paren prec 2 ((aux 2 f)^"/"^(aux 3 g))
        | Pow (f, g) -> "p("^(aux 0 f)^","^(aux 0 g)^")"
        | Max (f, g) -> ">("^(aux 0 f)^","^(aux 0 g)^")"
        | Indic (f, g) -> "=("^(aux 0 f)^","^(aux 0 g)^")"
        | Neg (f) -> "-"^(aux 2 f)
        | Abs (f) ->  "a("^(aux 0 f)^")"
        | Inv (f) ->  "v("^(aux 0 f)^")"
        | Exp (f) ->  "e("^(aux 0 f)^")"
        | Log (f) ->  "l("^(aux 0 f)^")"
        | Const x -> N.to_string x
        | Var (b,i) -> 
	    (if b then "y" else "x")^"["^(string_of_int i)^"]"
	| Symm (op, a) -> 
	    match op with
	      | SBSum -> "_+"^(aux 4 a)
	      | SBProd -> "_*"^(aux 4 a)
	      | SBMax -> "_>"^(aux 4 a)
	      | SBIndic -> "_="^(aux 4 a)
    in 
    aux 0 exp

  (* this function wraps in parens unless you really don't need to.*)
  let to_string_safe exp =
    let wrap_paren wrap str =
      (if wrap then "(" else "")
      ^str
      ^(if wrap then ")" else "")
    in
    let rec aux wrap exp =     (* prec is the current precedence *)
      match exp with
        | Sum(f, g) ->
            wrap_paren wrap ((aux true f)^"+"^(aux true g))
        | Diff (f, g) -> 
            wrap_paren wrap ((aux true f)^"-"^(aux true g))
        | Prod(f, g) ->
            wrap_paren wrap ((aux true f)^"*"^(aux true g))
        | Quot(f, g) ->
            wrap_paren wrap ((aux true f)^"/"^(aux true g))
        | Pow (f, g) -> "p("^(aux false f)^","^(aux false g)^")"
        | Max (f, g) -> ">("^(aux false f)^","^(aux false g)^")"
        | Indic (f, g) -> "=("^(aux false f)^","^(aux false g)^")"
        | Neg (f) -> "-"^(aux true f)
        | Abs (f) ->  "a("^(aux false f)^")"
        | Inv (f) ->  "v("^(aux false f)^")"
        | Exp (f) ->  "e("^(aux false f)^")"
        | Log (f) ->  "l("^(aux false f)^")"
        | Const x -> N.to_string x
        | Var (b,i) -> 
	    (if b then "y" else "x")^"["^(string_of_int i)^"]"
	| Symm (op, a) -> 
	    match op with
	      | SBSum -> "_+"^(aux true a)
	      | SBProd -> "_*"^(aux true a)
	      | SBMax -> "_>"^(aux true a)
	      | SBIndic -> "_="^(aux true a)
    in 
    aux false exp
		
  let of_string str = 
    let lexer = Genlex.make_lexer ["+";"-";"*";"/";
				   "(";")";",";"[";"]";
				   "p"; "a"; "v"; "e"; "l";
				   "x"; "y";
				   "_"; "="; ">" ] in
    let rec parse_expr = parser
	[< e1 = parse_mult; e = parse_more_adds e1 >] -> e
    and parse_more_adds e1 = parser
	[< 'Genlex.Kwd "+"; e2 = parse_mult; 
	   e = parse_more_adds (Sum(e1, e2)) >] -> e
      | [< 'Genlex.Kwd "-"; e2 = parse_mult; 
	   e = parse_more_adds (Diff(e1, e2)) >] -> e
      | [< >] -> e1
    and parse_mult = parser
	[< e1 = parse_simple; e = parse_more_mults e1 >] -> e
    and parse_more_mults e1 = parser
	[< 'Genlex.Kwd "*"; e2 = parse_simple; 
	   e = parse_more_mults (Prod(e1, e2)) >] -> e
      | [< 'Genlex.Kwd "/"; e2 = parse_simple; 
	   e = parse_more_mults (Quot(e1, e2)) >] -> e
      | [< >] -> e1
    and parse_simple = parser
      | [< 'Genlex.Float x >] -> of_float x
      | [< 'Genlex.Int n >] -> of_int n
      | [< 'Genlex.Kwd "x"; 'Genlex.Kwd "["; 
	   'Genlex.Int i ; 'Genlex.Kwd "]" >] -> Var(false,i)
      | [< 'Genlex.Kwd "y"; 'Genlex.Kwd "["; 
	   'Genlex.Int i ; 'Genlex.Kwd "]" >] -> Var(true,i)
      | [< 'Genlex.Kwd "("; e = parse_expr; 'Genlex.Kwd")" >] -> e
      | [< 'Genlex.Kwd ">"; 'Genlex.Kwd "("; e1 = parse_expr; 
	   'Genlex.Kwd ","; e2 = parse_expr; 'Genlex.Kwd")" >] -> Max(e1,e2)
      | [< 'Genlex.Kwd "p"; 'Genlex.Kwd "("; e1 = parse_expr; 
	   'Genlex.Kwd ","; e2 = parse_expr; 'Genlex.Kwd")" >] -> Pow(e1,e2)
      | [< 'Genlex.Kwd "="; 'Genlex.Kwd "("; e1 = parse_expr; 
	   'Genlex.Kwd ","; e2 = parse_expr; 'Genlex.Kwd")" >] -> Indic(e1,e2)
      | [< 'Genlex.Kwd "a"; 'Genlex.Kwd "("; e = parse_expr;
	   'Genlex.Kwd")" >] -> Abs(e)
      | [< 'Genlex.Kwd "-"; e = parse_simple >] -> Neg(e)
      | [< 'Genlex.Kwd "v"; 'Genlex.Kwd "("; e = parse_expr;
	   'Genlex.Kwd")" >] -> Inv(e)
      | [< 'Genlex.Kwd "e"; 'Genlex.Kwd "("; e = parse_expr;
	   'Genlex.Kwd")" >] -> Exp(e)
      | [< 'Genlex.Kwd "l"; 'Genlex.Kwd "("; e = parse_expr;
	   'Genlex.Kwd")" >] -> Log(e)
      | [< 'Genlex.Kwd "_"; e = parse_symm >] -> e
    and parse_symm = parser
      | [< 'Genlex.Kwd "+"; e = parse_simple >] -> Symm(SBSum,e)
      | [< 'Genlex.Kwd "*"; e = parse_simple >] -> Symm(SBProd,e)
      | [< 'Genlex.Kwd ">"; e = parse_simple >] -> Symm(SBMax,e)
      | [< 'Genlex.Kwd "="; e = parse_simple >] -> Symm(SBIndic,e)
    in
    let edited = 
      (* space out the symmetrizations so that parser doesn't get
	 confused *)
      Pcre.replace ~pat:"([>=])" ~templ:" $1 " 
	(* also make every -c a Neg(c) *)
	(Pcre.replace ~pat:"-" ~templ:" - " str) in
    try
      match lexer (Stream.of_string edited) with parser
	  [< e = parse_expr; _ = Stream.empty >] -> e
    with
        Stream.Error s -> 
	  failwith ("Algexp of_string error on "^edited)

  (* here we check that parsing works on unsafe before using it. we
     consider constants always to be positive, which explains the
     extend_neg *)
  let to_string exp = 
    let unsafe = to_string_unsafe exp in
    let ext = extend_neg exp in
    if of_string unsafe = ext then unsafe
    else (
      let safe = to_string_safe exp in
      if of_string safe <> ext then
	print_string ( "Algexp's of_string failed to parse " ^ safe ^ "\n");
      safe
    )
end


module TvsZ = Tvs(Number.Z)
module TvsR = Tvs(Number.R)

(*
open Algexp.TvsR
  let test s = TvsR.to_string ( TvsR.of_string s ) 
  let a = test "_>(x[0]-y[1])" 
  let b = test "v(x[0]-y[1])" 
*)


(*
let read s = Algexp.TvsR.of_string s 
  let b = read "v(x[0]-y[1])" 

let read s = Algexp.TvsR.of_string s 
let test s = Algexp.TvsR.to_string ( Algexp.TvsR.of_string s ) 
let test2 e = Algexp.TvsR.of_string ( Algexp.TvsR.to_string e )
let a = test "_=(y[1]+>(y[1],x[1]))"
let a = test "_=>(y[1],x[1])"
let q = Algexp.TvsR.const (-4.)
*)


