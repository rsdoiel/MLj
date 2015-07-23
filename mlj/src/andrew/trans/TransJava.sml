(*......................................................................*)
(* MLj - a Standard ML to Java bytecode compiler                        *)
(* Copyright (C) 1999 Persimmon IT Inc.                                 *)
(*                                                                      *)
(* This program is free software; you can redistribute it and/or        *)
(* modify it under the terms of the GNU General Public License          *)
(* as published by the Free Software Foundation; either version 2       *)
(* of the License, or (at your option) any later version.               *)
(*                                                                      *)
(* This program is distributed in the hope that it will be useful,      *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with this program; if not, write to the Free Software          *)
(* Foundation, Inc., 59 Temple Place - Suite 330, Boston,               *)
(* MA 02111-1307, USA.                                                  *)
(*......................................................................*)

(*======================================================================*)
(* Translation of MLj identifiers to MIL.				*)
(*======================================================================*)
structure TransJava =
struct

local 
  open MILTerm TransOps Effect Java
in

(*
fun makeFun (tabs : MILTerm.TAbstr as (typedvars,_), cty) = 
  let
    val f = freshVar ()
  in
    (
      Var f,
      MILTy.arrow (map #2 typedvars, cty),
      [(f, tabs)], 
      [] : (Var.Var * Val) list
    )
  end
*)

fun makeFun (f, eff) ty =
let
  val milty = TransType.transType TyVar.Map.empty TyName.Map.empty ty
  val SOME ([argty], cty) = MILTy.fromArrow milty
  val (_, restys) = MILTy.fromCmp cty
  val cty = MILTy.cmp(eff, restys)
  val v = freshVar ()
  val term = 
    case MILTy.fromProd argty of
      SOME comptys =>
      let
        fun makeTerm (0, args) = f (args Java((jop, NONE, NONE), args, cty)
          | makeTerm (i, args) = 
            let
              val v' = freshVar ()
            in
              LetVal(v', Proj(i-1, Var v), makeTerm(i-1, Var v' :: args))
            end
      in
        makeTerm (length comptys, [])
      end

    | NONE => 
      Java((jop, NONE, NONE), [Var v], cty)
in
  makeFun (([(v, argty)], term), cty)
end

val falseCmp = Triv [MILTermOps.falseVal]
val trueCmp  = Triv [MILTermOps.trueVal]

val javaOps = 
  map (fn (x, jop, eff) => (x, makeJavaFun (jop, eff)))
[
  (* Bytecodes *)
  ("add", 	 Add,    	none),
  ("And",  	 And, 		none),
  ("arraylength",ArrayLength, 	none),
  ("arrayload",  ArrayLoad, 	throws),
  ("arraystore", ArrayStore, 	throws),
  ("div", 	 Div, 	 	throws),
  ("eq",         Test Tests.eq, none),
  ("ge",     	 Test Tests.ge, none),
  ("gt",     	 Test Tests.gt, none),
  ("le",     	 Test Tests.le, none),
  ("lt",     	 Test Tests.lt, none),
  ("mul", 	 Mul, 	 	none),
  ("neg",     	 Neg, 	        none),
  ("newarray",   NewArray, 	union(throws, allocs)),
  ("or",   	 Or, 		none),
  ("rem", 	 Rem, 	        throws),
  ("shl",   	 Shl, 		none),
  ("shr",        Shr, 		none),
  ("sub", 	 Sub,    	none),
  ("ushr", 	 Ushr, 		none),
  ("xor",  	 Xor, 		none),

  (* Unsafe coercions *)
  ("toVector",   NopCast,       none),
  ("fromVector", NopCast,       none),

  (* Extra ops *)
  ("exnMessage", ExnMessage, 	none),
  ("exnName", 	 ExnName, 	none),
  ("isMLExn", 	 IsMLExn, 	none)
]

[ 
  (* Pure ML stuff *)
  ("=",    fn [a,b] => Cond(MLEq, a, b, trueCmp, falseCmp),      none),
  ("<>",   fn [a,b] => Cond(MLEq, a, b, falseCmp, trueCmp),      none),
  ("!",    fn [a]   => Deref (Var v),                            reads),
  ("ref",  fn [a]   => Alloc (AnyRef, a),                        allocs),
  (":=",   fn [a,b] => Let(Assign(a, b), ([], Triv [Tuple []])), writes)
end

val m = 
  foldl 
  (fn ((x, v), m) => Symbol.OrdMap.insert(m, Ids.symbol x, v))
  Symbol.OrdMap.empty

fun trans id = valOf(Symbol.OrdMap.find(m, id))

end