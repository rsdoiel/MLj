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
(* Convert MIL types to Java reps, generating classes as we go.		*)
(*======================================================================*)
structure CompileReps =
struct

(*----------------------------------------------------------------------*)
(* Types: for info, see signature					*)
(*----------------------------------------------------------------------*)
val prods = 
  ref (Stack.empty : JavaRep.Rep list Stack.stack)
val cons =
  ref (Stack.empty : JavaRep.Rep list Stack.stack)
val recs =
  ref (Stack.empty : (MILTy.Type * JavaRep.Rep option ref) Stack.stack)
val excons =
  ref (Stack.empty : (MILExn.Exn * JavaRep.Rep list * Syntax.longid)
    Stack.stack)
val classes =  
  ref (Stack.empty : TyName.TyName Stack.stack)

val classnames = 
  ref ([] : (TyName.TyName * string) list)

(* Do we need a top sum class? *)
val needsum = ref false

(* Do we need a top exn class? *)
val needexn = ref false

fun start () =
(
  prods := Stack.empty; 
  cons := Stack.empty;
  recs := Stack.empty;
  excons := Stack.empty;
  classes := Stack.empty;
  needsum := false;
  needexn := false
)

fun dump () =
(
  Debug.print "\nProduct classes:\n";
  Debug.print (Pretty.simpleVec "\n" 
    (fn (i, reps) => JavaNames.prodClassName i ^ " = (" ^
      Pretty.simpleVec "," JavaRepOps.toString reps ^ ")")
    (Gen.mapi Gen.id (Stack.listItems (!prods))));
  Debug.print "\nConstructor classes:\n";
  Debug.print (Pretty.simpleVec "\n" 
    (fn (i, reps) => JavaNames.conClassName (SOME i) ^ " = (" ^
      Pretty.simpleVec "," JavaRepOps.toString reps ^ ")")
    (Gen.mapi Gen.id (Stack.listItems (!cons))));
  Debug.print "\nException classes:\n";
  Debug.print (Pretty.simpleVec "\n" 
    (fn (i, (_,reps,longid)) => JavaNames.exnClassName (SOME i) ^ 
       " [" ^ Pretty.longidToString longid ^ "] = (" ^
      Pretty.simpleVec "," JavaRepOps.toString reps ^ ")")
    (Gen.mapi Gen.id (Stack.listItems (!excons))));
  Debug.print "\nInternal classes:\n";
  Debug.print (Pretty.simpleVec "\n" 
    (fn (i, tyname) => JavaNames.classClassName i ^ " = " ^ 
      TyName.toString tyname)
    (Gen.mapi Gen.id (Stack.listItems (!classes))));
  Debug.print "\nRecursive types:\n";
  Debug.print (Pretty.simpleVec "\n" 
    (fn (ty, ref (SOME rep)) => MILTy.toString ty ^ " |-> " ^ 
      JavaRepOps.toString rep)
    (Stack.listItems (!recs)))
)

fun finish save = 
(
  if save 
  then 
  (Stack.appi (fn a => SaveClass.save (CompileProdSum.makeProdClass a))   
    (!prods);
  Stack.appi (fn (i,reps) =>
    SaveClass.save (CompileProdSum.makeConClass(SOME i, reps))) (!cons);
  if !needsum then SaveClass.save (CompileProdSum.makeConClass (NONE,[]))
  else ();
  if Controls.isOn "showReps" then dump () else ()
  )
  else ();
  start ()
)

(*----------------------------------------------------------------------*)
(* Convert an exception name to a representation, adding it to the list *)
(* of exception constructors if necessary.                              *)
(*----------------------------------------------------------------------*)
fun exNameToRep (exname,reps,longid) =
  case Stack.find (fn (exname',_,_) => 
    MILExn.eq(exname, exname')) (!excons) of
    NONE =>
    let
      val i = Stack.numItems (!excons)
      val _ = 
        if !needexn then ()
        else (needexn := true; 
              SaveClass.save (CompileException.makeTopExnClass ()))            
    in
      excons := Stack.push(!excons, (exname,reps,longid));
      SaveClass.save (CompileException.makeExnClass(i,reps,longid));
      JavaRep.Exn (SOME i)
    end


  | SOME (i, _) =>
    JavaRep.Exn (SOME i)

(*
fun exnInfo env exname =
let
  val (exname, tys, classopt, longid) = MILTy.exNameInfo exname
in
  case classopt of
    NONE =>  
    (tys, exNameToRep (exname, map (tyToRep env) tys, longid))

  | SOME class => 
    (tys, JavaRep.Java (Types.CLASS class))
end
*)

(*----------------------------------------------------------------------*)
(* Convert a MIL type into its Java representation. This requires the	*)
(* representation environments.                                         *)
(*----------------------------------------------------------------------*)
and tyToRep (env : CompileOps.Env) ty =
  MILTy.deconstruct ty
  {
    tyvar =
      fn x => 
      case Var.Map.find(#kindenv env, x) of
        SOME (MILTy.Bound ty) => tyToRep env ty
      | _ =>
        Debug.fail 
        ("CompileReps.tyToRep: type variable: " ^ MILTy.toString ty),

    forall = fn (a as (tyvars,_)) => 
      let
        val tys = map (fn MILTy.Bound ty => ty) tyvars
      in
        tyToRep env (MILTy.app (MILTy.abs a, tys))
      end,

    java = JavaRep.Java,

    deb = fn _ => Debug.fail ("CompileReps.tyToRep: type-bound tyvar in: " ^ 
        MILTy.toString ty),

    tyname = fn n => 
      let
        fun find [] =
          (case Stack.find (fn n' => TyName.eq(n,n')) (!classes) of
     
            NONE => 
            (classes := Stack.push(!classes, n);
             JavaRep.Class (Stack.numItems (!classes) - 1))

          | SOME (i, _) => 
            JavaRep.Class i)

        | find ((n', s)::rest) =
          if TyName.eq(n, n')
          then  
            JavaRep.Java (Types.CLASS (ClassHandle.unknown 
              (JavaString.fromString s)))
          else find rest
    in
      find (!classnames)
    end,

  con = fn [] => (needsum := true; JavaRep.Con NONE)
  | tys =>
    let
      val reps = map (tyToRep env) tys
    in
      case Stack.find (fn reps'=>Eq.list JavaRepOps.eq (reps,reps')) (!cons) of
        NONE => 
        let
          val i = Stack.numItems (!cons)
        in
          cons := Stack.push(!cons, reps);
          JavaRep.Con (SOME i)
        end

      | SOME (i, _) =>
        JavaRep.Con (SOME i)
    end,
  
  exn = fn (exname, tys) => 
    exNameToRep (exname, map (tyToRep env) tys, MILExn.info exname),


  prod = fn 
    [] => 
    JavaRep.Object

  | tys =>
    let
      val reps = map (tyToRep env) tys
    in
      case Stack.find (fn r => Eq.list JavaRepOps.eq (reps,r)) (!prods) of     
        NONE => 
        let
          val i = Stack.numItems (!prods)
        in
          prods := Stack.push(!prods, reps);
          JavaRep.Prod i
        end

      | SOME (i, _) => 
        JavaRep.Prod i
    end,

  sum = fn tyss => 
    (if List.all null tyss then JavaRep.Java Types.INT  
    else case tyss of
      ([[], [ty]] | [[ty], []]) =>
      if MILTy.someIsNop (#kindenv env) ty
      then tyToRep env ty
      else tyToRep env (MILTy.prod [ty])

    | _ => (needsum := true; JavaRep.Con NONE)
    ),

  arrow = fn (tys1, cty) => JavaRep.Closure NONE,

  closure = fn (iopt, _) => JavaRep.Closure iopt,

  refty = fn ty => tyToRep env (MILTy.prod [ty]),

  array = fn ty => JavaRep.Array (tyToRep env ty),
  vector = fn ty => JavaRep.Array (tyToRep env ty),

  (* This is naughty and used to lead to loops *)
  mu = fn a =>
    let 
      (* Bounds must be forced because type will be looked at from unrelated
         context *)
      val ty = MILTy.forceBounds (#kindenv env) ty
    in
      case Stack.find (fn (ty', repref) =>
          case MILTy.lub (ty,ty') of
            NONE => false
          | SOME ty' => true) (!recs) of

        SOME (_,(_,repref)) => 
        JavaRep.RepRef repref

      | NONE =>
        let 
          val repref : JavaRep.Rep option ref = ref NONE
        in
          recs := Stack.push(!recs, (ty,repref));
          let
            val rep = tyToRep env (MILTy.unfold a)
          in
            repref := SOME rep; 
            rep
          end
        end
    end
  }

fun externalTyToRep (env : CompileOps.Env) ty =
if MILTy.eq (ty, MILTys.bool)
then JavaRep.Java Types.BOOLEAN
else tyToRep env ty


end
