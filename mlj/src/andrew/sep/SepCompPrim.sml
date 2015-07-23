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
(* Primitive structures (at present, just Prim)                         *)
(*======================================================================*)
structure SepCompPrim :> SEPCOMPPRIM =
struct

local 
  open MILTerm TransOps Effect Java
in

(*----------------------------------------------------------------------*)
(* Make a typed abstractions for a particular type.    		        *)
(*                                                                      *)
(* The function f takes lists of value arguments and result types and   *)
(* returns a computation term.                                          *) 
(*----------------------------------------------------------------------*)
fun makeFun f milty =
let
  val SOME ([argty], cty) = MILTy.fromArrow milty
  val (_, restys as [resty]) = MILTy.fromCmp cty
  val restys =
    case MILTy.fromProd resty of
      SOME [] => []
    | _ => restys

  val x = freshVar ()
  val term = 
    case MILTy.fromProd argty of
      SOME comptys =>
      let
        fun makeTerm (0, args) = 
            f (args, restys)

          | makeTerm (i, args) = 
            let
              val x' = freshVar ()
            in
              LetVal(x', Proj(i-1, Var x), makeTerm(i-1, Var x' :: args))
            end
      in
        makeTerm (length comptys, [])
      end

    | NONE => 
      f ([Var x], restys)

  val term = 
    case MILTy.fromProd resty of
      SOME [] => Let(term, ([], Triv [Tuple []])) 
    | _ => term
in
  ([(x, argty)], term)
end

val falseCmp = Triv [MILTermOps.falseVal]
val trueCmp  = Triv [MILTermOps.trueVal]

fun makeJavaFun (j, eff) = 
  makeFun (fn (vs, restys) => Java((j, NONE, NONE), vs, MILTy.cmp(eff,restys)))

fun makeJavaTest t =
  makeFun (fn ([a, b], restys) => Cond(JavaTest t, a, b, trueCmp, falseCmp))

val JavaOps = 
  map (fn (x, jop, eff) => (x, makeJavaFun (jop, eff)))
[
  (* Bytecodes *)
  ("add", 	 Add,    	none),
  ("And",  	 And, 		none),
  ("arraylength",ArrayLength, 	none),
  ("arrayload",  ArrayLoad, 	throws),
  ("arraystore", ArrayStore, 	throws),
  ("cmp",        CmpL,          none),
  ("cmpl",       CmpL,          none),
  ("cmpg",       CmpG,          none),
  ("div", 	 Div, 	 	throws),
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

  (* Extra ops *)
  ("exnMessage", ExnMessage, 	none),
  ("exnName", 	 ExnName, 	none),
  ("isMLExn", 	 IsMLExn, 	none)
]


val JavaTestOps = 
  map (fn (x, t) => (x, makeJavaTest t))
[
  ("eq",         Tests.eq),
  ("ge",     	 Tests.ge),
  ("gt",     	 Tests.gt),
  ("le",     	 Tests.le),
  ("lt",     	 Tests.lt)
]


val coercions = 
  map (fn x => (x, makeFun (fn ([a], [ty]) => Triv [Coerce(a, ty)])))
[
  "toVector", "fromVector", "unsafeValOf", "fromWord", "toWord",
  "fromWord64", "toWord64", "fromWord8", "toWord8"
]

val casts = map (fn x => (x, makeJavaFun (Cast, none)))
[
  "d2f", "d2i", "d2l",
  "f2d", "f2i", "f2l",
  "i2b", "i2c", "i2d", "i2f", "i2l", "i2s",
  "l2d", "l2f", "l2i",
  "c2i", "b2i", "s2i"
]

val MLOps = 
  map (fn (x, f) => (x, makeFun (fn (args, eff) => f args)))
[ 
  (* Pure ML stuff *)
  ("=",    fn [a,b] => Cond(MLEq, a, b, trueCmp, falseCmp)),
  ("<>",   fn [a,b] => Cond(MLEq, a, b, falseCmp, trueCmp)),
  ("ref",  fn [a]   => Alloc (AnyRef, a)),
  ("!",    fn [a]   => Deref a),
  (":=",   fn [a,b] => Assign(a, b))
]

val allOps = 
  foldl 
  (fn ((x, v), m) => Symbol.OrdMap.insert(m, Ids.symbol x, v))
  Symbol.OrdMap.empty
  (JavaOps @ JavaTestOps @ MLOps @ coercions @ casts)


fun makePrimTerm (TNE,E) = 
let
  val VE = EnvOps.VEofE E
  fun lookup (id, TVE) smlty = 
  let
    val milty = TransType.transType TVE TNE smlty
    val SOME makeTAbs = Symbol.OrdMap.find(allOps, id)
    val tabs = makeTAbs milty
    val f = freshVar ()
  in
    (f, tabs)
  end
 
  fun trans ([], args) = 
      Triv [Tuple (rev args)]

    | trans ((id, ValBind.VarSch sch)::rest, args) = 
      (case sch of
        SMLSch.TypeScheme([], smlty) =>
        let
          val (f, tabs) = lookup (id, TyVar.Map.empty) smlty
        in
          LetFun([], AnyFun, Fun(f, tabs), trans (rest, Var f :: args))
        end                  

      | SMLSch.TypeScheme(smltyvars, smlty) =>
        case map TyVar.sort smltyvars of
          [TyVar.Overloaded tynames] =>
          let
            val tyvar = hd smltyvars
            val smltys = map (fn tyname => 
              (SMLTy.appSubst [(tyvar, SMLTy.consType([], tyname))] smlty))
              (TyName.Set.listItems tynames)

            val pairs = map (lookup (id, TyVar.Map.empty)) smltys
            val p = freshVar ()

            fun trans' ([], args') =
                LetVal(p, Tuple (rev args'), trans (rest, Var p :: args))

              | trans' ((f, tabs)::rest', args') =
                LetFun([], AnyFun, Fun(f, tabs), trans'(rest', Var f :: args'))
                
          in
            trans' (pairs, [])
          end

        | _ =>
          let
            val (TVE,tyvars) = TransOps.freshTyVars (TyVar.Map.empty,smltyvars)
            val (f, tabs) = lookup (id, TVE) smlty
          in
            LetFun(tyvars, AnyFun, Fun(f, tabs), trans (rest, Var f :: args))
          end
      )

    | trans (_::rest, args) = trans(rest, args)
in
  trans (Symbol.OrdMap.listItemsi VE, [])
end

fun makePrimEntry (TNE,primE) = 
let
  val _ = (TransOps.vs := Var.initial; TransOps.tvs := Var.initial)
  val ty = TransType.transE TNE primE
  val term = makePrimTerm (TNE,primE)
in
  SepCompTypes.Str
  {
    E = primE,

    CE = TyName.Map.empty,

    EE = IMap.empty,

    strVars = Symbol.OrdMap.empty,

    (* Maximum variable index used for structure IDs *)
    limit = 0, 

    (* The MIL variable supply *)
    supply = !TransOps.vs,

    (* The MIL term *)
    term = term,

    (* MIL type defs for this module *)
    tynameTys = TyName.Map.empty,

    (* The type of the MIL term *)
    ty = ty
  } : SepCompTypes.Result
end

end (* of local open *)

end