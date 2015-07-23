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

structure ClosConvTypes =
struct

type ClosDef = 
  {
    fvtys : MILTy.Type list,    (* Upper bound on types of free variables *)
    meths : 
    {
      tyvars: (Var.Var*MILTy.Kind) list, (* Method might be polymorphic *)
      fvtys : MILTy.Type list,      (* Actual types for fvs for this method *)
      fundef: Var.Var * MILTerm.TAbstr * MILTy.CmpType
    } IMap.map
  }

(*----------------------------------------------------------------------*)
(* The result type of closure conversion				*)
(*----------------------------------------------------------------------*)
type Result =
{
  (* Global function definitions with local blocks required *)
  fundefs : ((Var.Var*MILTy.Kind) list * Var.Var * MILTerm.TAbstr) list,

  (* Global function environment mapping names to types & function numbers *)
  funenv  : (int * MILTy.Type) Var.Map.map,

  (* Global variable types *)
  globvars : MILTy.Type Var.Map.map,

  (* User class definitions with local blocks required *)
  classdefs: 
    (MILTy.Type * MILTerm.ClassInfo * MILTerm.FieldInfo list * 
     MILTerm.MethodInfo list) list,

  (* Closure classes with local blocks required *)
  (* The i'th element of the list is closure class number i *)
  closdefs : ClosDef list,

  (* App methods required in top function class *)
  appmeths : int Var.Map.map,

  (* Types of app methods *)
  methtys : MILTy.Type list,

  (* The clinit term with local blocks required *)
  clinit : MILTerm.Cmp
}

end
