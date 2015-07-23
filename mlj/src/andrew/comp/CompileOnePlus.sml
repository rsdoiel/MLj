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
(*======================================================================*)
structure CompileOnePlus :> COMPILEONEPLUS =
struct

local open CompileOps CompileReps
in

val nones =
  ref (MILTy.Map.empty : (int * JavaRep.Rep * JavaRep.Rep list) MILTy.Map.map)

val count = 
  ref 0

(*----------------------------------------------------------------------*)
(* Lookup the dummy value for a 1+ type.				*)
(* Create a new one if necessary.                                       *)
(*----------------------------------------------------------------------*)
fun lookupNone (ty, rep) =
  case MILTy.Map.find(!nones, ty) of
    NONE =>
    let
      val i = !count
      val argreps =
        case JavaRepOps.normalise rep of 
          JavaRep.Prod p =>
          Stack.nth(!CompileReps.prods, p)

        | JavaRep.Con NONE =>
          [JavaRep.Java Types.INT]

        | _ => 
          []     
    in
      count := i + 1;
      nones := MILTy.Map.insert(!nones, ty, (i, rep, argreps)); i
    end
 
  | SOME(i,_,_) => i

(*----------------------------------------------------------------------*)
(* Given a type ty, return the instructions and value necessary to	*)
(* generate inl <> : 1+ty                                               *)
(*----------------------------------------------------------------------*)
fun none env ty = 
let
  val ty = MILTy.forceBounds (#kindenv env) ty
in
  if MILTy.noneIsNull (#kindenv env) ty
  then ([], constNull)
  else
    let
      val rep = 
        if MILTy.someIsNop (#kindenv env) ty 
        then tyToRep env ty
        else tyToRep env (MILTy.prod [ty])
      val i = lookupNone (ty, rep)
      val value = Blocks.new_value ()
    in
      ([getstatic(JavaNames.globalClass, 
        JavaString.fromString (JavaNames.noneVal i), 
        true, (value, rep))], value)
    end
end

(*----------------------------------------------------------------------*)
(* Given a type ty and a value for v, return the instructions and value	*)
(* necessary to generate inr <v> : 1+ty                                 *)
(*----------------------------------------------------------------------*)
fun some env (ty, value) =
let
  val ty = MILTy.forceBounds (#kindenv env) ty
in
  if MILTy.someIsNop (#kindenv env) ty
  then ([], value)
  else 
      let
        val rep = tyToRep env (MILTy.prod [ty])
        val argrep = tyToRep env ty
        val result = Blocks.new_value ()
      in
        ([init ([(value,argrep)], (result, rep))], result)
      end
end

(*----------------------------------------------------------------------*)
(* Given a type ty and a value for inr <v>, return the instructions and *)
(* value corresponding to v that are required for the projection.       *)
(*----------------------------------------------------------------------*)
fun proj env (ty, value) =
let
  val ty = MILTy.forceBounds (#kindenv env) ty
in
    if MILTy.someIsNop (#kindenv env) ty
    then ([], value)
    else
    let
      val result = Blocks.new_value ()
      val rep = tyToRep env (MILTy.prod [ty])
      val fldrep = tyToRep env ty
    in
      ([CompileOps.proj ((value, rep), JavaString.fromString (JavaNames.argLabel 0), true, 
        (result, fldrep))], result)
    end
end

(*----------------------------------------------------------------------*)
(* Generate the instructions necessary to create a single dummy none.	*)
(*----------------------------------------------------------------------*)
fun makeNoneInstrs (i, rep, argreps) =
    let
      val resultvalue = Blocks.new_value ()
    in
      [(case rep of
         JavaRep.Array rep' =>
         instr(
           Blocks.newarray(JavaRepOps.toJava rep, Blocks.MUSTDO), 
           [constInt 0], [resultvalue])    
  
       | _ => 
         instr(
           Blocks.new(mref (JavaRepOps.toClass rep, JavaString.fromString "<init>", argreps, NONE)), 
           map (constant o JavaRepOps.nullValue) argreps, 
           [resultvalue])
       ),
       putstatic (JavaNames.globalClass, JavaString.fromString (JavaNames.noneVal i), true, 
         (resultvalue, rep))
      ]
    end

(*----------------------------------------------------------------------*)
(* Generate the instructions necessary to create all dummy none values.	*)
(*----------------------------------------------------------------------*)
fun makeNones () =
  List.concat (map makeNoneInstrs (MILTy.Map.listItems (!nones)))

(*----------------------------------------------------------------------*)
(* Make a field defn for a single dummy none.                           *)
(*----------------------------------------------------------------------*)
fun makeNoneField (i, rep, _) =
      Field.simple
      {
        name = JavaString.fromString (JavaNames.noneVal i), 
        flags = [Field.PROTECTED, Field.STATIC], 
        field_type = JavaRepOps.toJava rep,
        attributes = Attribute.empty
      }

(*----------------------------------------------------------------------*)
(* List the global fields used for all dummy none values.		*)
(*----------------------------------------------------------------------*)
fun makeNoneFields () = 
  map makeNoneField (MILTy.Map.listItems (!nones))

fun init () = (nones := MILTy.Map.empty; count := 0)


      
end (* of local open *)

end (* of struct *)
