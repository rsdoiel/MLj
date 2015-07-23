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
(* Linking of modules together.  					*)
(*======================================================================*)
structure Link :> LINK =
struct

local 
  open MILTerm SepCompTypes
in

(*----------------------------------------------------------------------*)
(* Gather up concrete type definitions					*)
(*----------------------------------------------------------------------*)
fun makeTyMap entities =
let
  fun add ([], tymap) = tymap

    | add (entity::entities, tymap) =

      case (entity, Entity.Map.find(!SepComp.cache, entity)) of
        ((Entity.Str, strid), SOME (Str {tynameTys = TNE, ...}, _)) =>
        let
          val f = MILTy.replace tymap
        in
          add (entities, 
            TyName.Map.foldri (fn (tyname, ty, tymap) =>
              MILTy.Map.insert(tymap, MILTy.tyname tyname, f ty)) tymap TNE)
        end

      | _ =>
        add (entities, tymap)
in
  add (entities, MILTy.Map.empty)
end

(*----------------------------------------------------------------------*)
(* Rename bound variables in term by adding an increment;               *)
(* rename free variables in term using map.                             *)
(*----------------------------------------------------------------------*)
fun rename (limit,m,t,offset) e =
let 
  fun r x = 
    if x >= limit then x + offset
    else case Var.Map.find(m,x) of
      NONE => Debug.fail ("Link.rename: illegal variable: " ^ Var.toString x
              ^ " with limit " ^ Var.toString limit)
    | SOME x' => x'

  fun rv v =
  case v of
    Var x => Var (r x)
  | SCon _ => v
  | Inj(ty, i, args) => Inj(t ty, i, map rv args)
  | Coerce(v, ty) => Coerce(rv v, t ty)
  | ExCon(excon, args) => ExCon(excon, map rv args)
  | Tuple args => Tuple (map rv args)
  | Proj(i, v) => Proj(i, rv v)
  | TApp(v, tys) => TApp(rv v, map t tys)
  | TAbs(tyvars, v) => TAbs(tyvars, rv v)
  | Fold (v, ty) => Fold(rv v, t ty)
  | Unfold v => Unfold(rv v)

  and rtabs (typedvars, e) = 
    (map (fn (x,ty) => (r x, t ty)) typedvars, re e)

  and rabs (xs, e) = (map r xs, re e)

  and re e =
  let
    fun rc (v, bindargs, cases, eopt) = 
      (rv v, bindargs, 
        map (fn (i, abs) => (i, rabs abs)) cases, Option.map re eopt)

  in
  case e of
    App(v, vs) => App(rv v, map rv vs)
  | Java(j, vs, cty) => Java(j, map rv vs, cty)
  | Let(e, abs) => Let(re e, rtabs abs)
  | Triv vs => Triv (map rv vs)
  | Case cases => Case(rc cases)
  | CaseSCon cases => CaseSCon(rc cases)
  | CaseExCon cases => CaseExCon(rc cases)
  | Cond(t,v1,v2,e1,e2) => Cond(t, rv v1, rv v2, re e1, re e2)
  | Throw(v, tys, loc) => Throw(rv v, map t tys, loc)
  | TryLet(e, handlers, abs) => TryLet(re e, map rtabs handlers, rtabs abs)
  | LetFun(tyvars, kind, def, e) => 
    LetFun(tyvars, kind, 
      case def of
        RecFun recbinds =>
        RecFun (map (fn (f,g,tabs,cty) => (r f,r g,rtabs tabs,cty)) recbinds)
      | Fun (f, tabs) =>
        Fun (r f, rtabs tabs), re e)
  | LetClass(name,info,fields,methods,e) => 
    LetClass(name,info,fields,
      map (fn (n,ms,tys,tyopt,absopt) => 
        (n,ms,tys,tyopt, case absopt of NONE => NONE
          | SOME (f, abs) => SOME (r f, rabs abs))) methods, re e)
  | Deref v => Deref(rv v)
  | Alloc (f,v) => Alloc(f, rv v)
  | Assign(v1,v2) => Assign(rv v1, rv v2)
  | LetVal(x, v, e) => LetVal(r x, rv v, re e)
  | Encap e => Encap(re e)
  end
in
  re e
end


fun link (entities : Entity.Ref list, names) = 
let
  val info = List.mapPartial 
    (fn entity =>
     case (entity, Entity.Map.find(!SepComp.cache, entity)) of
      ((Entity.Str, strid), 
        SOME (Str { strVars, limit, term, ty, supply, ...}, _)) =>
        SOME (strid, strVars, limit, term, ty, supply)
      | _ => NONE) entities

  val tymap = makeTyMap entities

  val names = 
    map (fn (tyname,s) => 
      ((case MILTy.Map.find(tymap, MILTy.tyname tyname) of
        SOME ty =>
        (case MILTy.fromTyname ty of
          SOME tyname => tyname
        | NONE => tyname)
      | NONE => tyname), s)) names

  val (SE,supply) = 
    foldr (fn ((strid, _, _, _, _, _), (SE, supply)) =>
      let val (supply', globalvar) = Var.fresh supply
      in (Symbol.OrdMap.insert(SE, strid, globalvar), supply') end) 
      (Symbol.OrdMap.empty,Var.initial) info

  val t = MILTy.replace tymap

  fun link' (e,supply) [] = 
      (e,supply)

    | link' (e,supply) ((strid, SE', limit, e', ty, supply')::info)=
      let  
        val r = Symbol.OrdMap.foldri (fn (strid, localvar, r) => 
          let val globalvar = valOf(Symbol.OrdMap.find(SE, strid))
          in Var.Map.insert(r, localvar, globalvar) end) 
            (Var.Map.insert(Var.Map.empty, Var.dummy, Var.dummy))
          SE'

        val e' = rename (limit, r, t, supply) e'
        val globalvar = valOf(Symbol.OrdMap.find(SE, strid))
      in
        link' 
          (Let(e', ([(globalvar,ty)], e)),
           supply + supply') info
      end

  val (e,supply) = link' (Triv [], supply) info

  (* Put a top-level exception handler round the whole thing *)
  val (e,supply) = 
    if Controls.isOn "exnLocs"
    then
    let 
      val (supply', exnvar) = Var.fresh supply
    in
      (TryLet(e, [([(exnvar, MILTys.topExn)], 
        Java((Java.Invoke, NONE,SOME(JavaString.fromString "printStackTrace")),
          [Coerce(Var exnvar, MILTy.java 
            (Types.CLASS ClassHandle.throwable))],
          MILTy.cmp(Effect.any, [])))], ([], Triv [])), supply')
    end else (e, supply)

in
  (e,supply,names)
end

end (* of local open *)

end (* of struct *)
