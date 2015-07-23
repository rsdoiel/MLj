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
(* For functions that aren't already completely local, create an inner	*)
(* block in order to implement tail calls.                              *)
(*======================================================================*)
structure TailRec :> TRANSFORMER =
struct


local 
  open MILTerm Counters
in

fun transform (tyenv) e =
let

(*----------------------------------------------------------------------*)
(* Tail variable info.							*)
(*----------------------------------------------------------------------*)
val tails = ref (Var.Map.empty : Var.Var Var.Map.map)

(*----------------------------------------------------------------------*)
(* Cons-tail variable info.						*)
(*----------------------------------------------------------------------*)
val constails = ref (Var.Map.empty : (Var.Var*Var.Var*MILTy.Type) Var.Map.map)

(*----------------------------------------------------------------------*)
(* Generate a new tail variable if there isn't already one there.       *)
(*----------------------------------------------------------------------*)
fun addTail g =
  case Var.Map.find(!tails, g) of

    (* If it's already been used in a tail-recursive app then just return *)
    (* its inner name. *)
    SOME g' =>
    (Census.addVar(g,~1); Census.addVar(g', 1); g')

    (* Otherwise generate a fresh name, record in the state and return it *)
  | NONE =>
    let
      val g' = Census.freshVar 1
    in
      Census.addVar(g,~1);
      tails := Var.Map.insert(!tails, g, g'); 
      g'
    end

(*----------------------------------------------------------------------*)
(* Generate a new cons tail variable if there isn't already one there.  *)
(*----------------------------------------------------------------------*)
fun addConsTail (g,muty) =
  case Var.Map.find(!constails, g) of

    SOME (g',extra,_) =>
    (Census.addVar(g,~1); Census.addVar(g', 1); Census.addVar(extra,1); 
    (g',extra))

  | NONE =>
    let
      val g' = Census.freshVar 1
      val extra = Census.freshVar 1
    in
      Census.addVar(g,~1);
      constails := Var.Map.insert(!constails, g, (g',extra,muty)); 
      (g',extra)
    end

(*----------------------------------------------------------------------*)
(* Do the translation on computation terms.				*)
(*----------------------------------------------------------------------*)
fun transCmp tailvar e =
let
  val tt = transCmp tailvar
  fun transCases (v, bindargs, cases, e) = 
    (v, bindargs, map (fn (i,(xs,e)) => (i,(xs, tt e))) cases, Option.map tt e)
in
  case e of
  App(Var x, vs) =>
  (case tailvar of
    NONE => e
  | SOME x' => 
    if Var.eq(x, x') andalso enabled tailRec
    then App(Var (addTail x), vs)
    else e)

| Encap e => 
  Encap (tt e)

| Let(e1, (typedvars, e2)) =>
  let
    fun default () = Let(transCmp NONE e1, (typedvars, tt e2))
  in
    case (e1, (typedvars, e2)) of
      (App(Var f, vs), ([(x, ty)], 
      LetVal(y, Tuple vs', Triv[Fold(v' as Inj(sumty, _,[Var y']),muty)]))) =>
      if Var.eq(y,y') 
      andalso (case tailvar of SOME f' => Var.eq(f,f') | NONE => false)
      then
      case (rev vs') of
        Var x'::vs'' =>
        if Var.eq(x,x') andalso enabled tailCons
        then
        let
          val (g', extra) = addConsTail (f, muty)
          val prodty = 
            case MILTy.fromSum sumty of
              SOME ([[],[ty]] | [[ty],[]]) => ty
            | _ => Debug.fail "TailRec.transCmp: expected 1+ type"

          val SOME fldtys = MILTy.fromProd prodty
        in
          Census.addVar(y, 1);
          Census.addVar(x, ~1);
          LetVal(y, Tuple (rev (Null ty :: vs'')),
          Init(extra, length fldtys - 1, v', App(Var g', Var y :: vs)))
        end
        else default ()
      | _ => default ()
      else default ()

    | _ => default ()
  end

| LetVal(x, v, e) =>
  LetVal(x, v, tt e)

| Case a =>
  Case (transCases a)

| CaseSCon a =>
  CaseSCon (transCases a)
      
| CaseExCon a =>
  CaseExCon (transCases a)

| TryLet(e0, tabss, (vs, e2)) =>
  TryLet(transCmp NONE e0, map (fn (vs,e) => (vs,transCmp NONE e)) tabss,
    (vs, tt e2))

| Cond(t, v1, v2, e1, e2) =>
  Cond(t, v1, v2, tt e1, tt e2)
  
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun transMethod (method as (name, mods, tys, tyopt, optabs)) =
        case optabs of 
          NONE => method

        | SOME (f, (vs, e)) =>
          (name, mods, tys, tyopt, SOME (f, (vs, transCmp NONE e)))
  in
    LetClass(classname, classinfo, fields, map transMethod methods, tt e)
  end
  
| LetFun(tyvars, kind, def, e) =>
  let
    fun transDef (Fun (f, (typedvars, e))) =
        Fun (f, (typedvars, 
          if MILTermOps.isLocal kind then tt e else transCmp NONE e))

      | transDef (RecFun recbinds) =
        RecFun (map (fn (f, g, (typedvars, e), cty) =>
        let
          (* Don't do tail recursion transformation if it's a local block
             or the appropriate flags aren't set *)
          val e = 
            if MILTermOps.isLocal kind orelse not (Controls.isOn "tailRec"
              orelse Controls.isOn "tailCons") 
            then tt e 
            else transCmp (SOME g) e

          val e = 

            (* Were there any tail-cons transformations? *)
            case Var.Map.find(!constails, g) of
              SOME (g', extra, muty) =>
              let
                val a = 
                  case MILTy.fromMu muty of
                    NONE => Debug.fail "TailRec.transCmp: expected mu type"
                  | SOME a => a
                
                val sumty = MILTy.unfold a
                val prodty = 
                  case MILTy.fromSum sumty of
                    SOME ([[], [ty]] | [[ty], []]) => ty
                  | _ => Debug.fail "TailRec.transCmp: expected 1+ type"

                val fldtys = valOf(MILTy.fromProd prodty)
                val i = length fldtys - 1

                val f' = Census.freshVar 1
                val extra' = Census.freshVar 2

                val x = Census.freshVar 1
                val f'' = Census.freshVar 0
                val finishfundef = 
                  Fun (f'', ([(x,muty)], Init(extra, i, Var x, 
                        Triv [Proj(i, Var extra')])))

                val locals = Var.Set.add(Var.Set.add(Var.Set.empty, g), g')

                val e = 
                  case Var.Map.find(!tails, g) of
                    NONE => 
                    (Census.addVar(extra, 1);
                    MILTermOps.addContinuation (locals, f'', cty) e)

                  | SOME h =>
                    let
                      val typedvars' = 
                        map (fn (v,ty) => (Census.freshVar 1,ty)) typedvars
                      val e = MILTermOps.addContinuation 
                        (Var.Set.add(locals,h), f'',cty) e
                    in
                      Census.addVar(g', 1);
                      Census.addVar(extra, 2);
                      LetFun([], LocalFun, 
                        Fun (h,(typedvars', App(Var g', 
                          Var extra::map (Var o #1) typedvars'))), e)
                    end

              in
                app (fn (v,_) => Census.addVar(v,1)) typedvars;      
                LetVal(extra', Tuple (map Null fldtys),
                LetFun([], LocalFun, 
                  RecFun [(f',g',((extra,prodty)::typedvars,
                  LetFun([], LocalFun, finishfundef, e)), cty)], 
                  App(Var f',Var extra'::map (Var o #1) typedvars)))
              end

            | NONE => 
              (case Var.Map.find(!tails, g) of
                SOME g' =>                 
                let
                  val f' = Census.freshVar 1
                in
                  app (fn (v,_) => Census.addVar(v,1)) typedvars;
                  LetFun([], LocalFun, RecFun [(f',g',(typedvars,e),cty)],
                    App(Var f', map (Var o #1) typedvars))
                end

              | NONE => e)

        in
          (f, g, (typedvars, e), cty)
        end) recbinds)

  in
    LetFun(tyvars, kind, transDef def, tt e)
  end

| _ => e

end

val _ = Counters.reset ()
val e = transCmp NONE e
in
  Counters.printCounts(); e
end

end (* of local open *)
end (* of struct *)

