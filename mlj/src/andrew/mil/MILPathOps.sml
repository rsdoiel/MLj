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
(* Path operations.                                                     *)
(*======================================================================*)
structure MILPathOps :> MILPATHOPS =
struct

local 
  open MILPath
in

(*----------------------------------------------------------------------*)
(* Pretty-print a path.  						*)
(*----------------------------------------------------------------------*)
local
  fun itemToString (LetFun v) = Var.toString v
    | itemToString (LetClass v) = "method " ^ Var.toString v
    | itemToString (Cond b) = if b then "then" else "else"
    | itemToString (CaseCon iopt) = 
      (case iopt of NONE => "_" | SOME i => Int.toString i)
    | itemToString (CaseSCon copt) = 
      (case copt of NONE => "_" | SOME c => Constants.constant_toString c)
    | itemToString (CaseExCon eopt) = 
      (case eopt of NONE => "_" | SOME e => MILTy.toString e)
in
  fun toString items = Pretty.simpleVec "/" itemToString (rev items)
end

fun eqItem (LetFun f, LetFun g) = Var.eq(f,g)
  | eqItem (LetClass f, LetClass g) = Var.eq(f,g)
  | eqItem (Cond b1, Cond b2) = b1=b2
  | eqItem (CaseCon i1, CaseCon i2) = i1=i2
  | eqItem (CaseSCon i1, CaseSCon i2) = 
    Eq.option (fn (c1,c2) => Constants.equal (c1,c2,false)) (i1,i2)
  | eqItem (CaseExCon e1, CaseExCon e2) = Eq.option MILTy.eq (e1,e2)
  | eqItem _ = false

(*----------------------------------------------------------------------*)
(* Equality on paths							*)
(*----------------------------------------------------------------------*)
fun eq (path1, path2) = Eq.list eqItem (path1,path2)

(*----------------------------------------------------------------------*)
(* Largest common prefix						*)
(*----------------------------------------------------------------------*)
fun join (path1, path2) =    
let
  fun loop prefix (item1::items1, item2::items2) =
      if eqItem (item1,item2) then loop (item1::prefix) (items1,items2)
      else prefix
    | loop prefix _ = prefix
in
  loop [] (rev path1,rev path2)
end

structure Map =
struct

(*----------------------------------------------------------------------*)
(* Datatype for trees, used for mapping paths to scopes.		*)
(*----------------------------------------------------------------------*)
datatype 'a Branch = 
  LetFunNode of 'a map Var.Map.map * 'a Branch
| LetClassNode of 'a map Var.Map.map * 'a Branch
| CondNode of 'a map * 'a map
| CaseConNode of 'a map IMap.map * 'a map 
| CaseSConNode of 'a map CMap.map * 'a map 
| CaseExConNode of 'a map MILTy.Map.map * 'a map
| Empty

withtype 'a map = 'a list * 'a Branch

val empty = ([], Empty)

fun treeToString depth f (a, br) =
Pretty.newline depth ^ "[" ^ Pretty.simpleVec "," f a ^ "]" ^ 
branchToString depth f br  

and branchToString depth f br =
case br of
  LetFunNode(funs,body) => 
  "letfun " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "and ")
  (fn (x,tree) => Var.toString x ^ ":" ^ treeToString (depth+1) f tree)
  (Var.Map.listItemsi funs) ^ Pretty.newline depth ^ "in " ^ 
    branchToString (depth+1) f body

| LetClassNode(meths,body) => 
  "letclass " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "and ")
  (fn (x,tree) => Var.toString x ^ ":" ^ treeToString (depth+1) f tree)
  (Var.Map.listItemsi meths) ^ Pretty.newline depth ^  "in " ^ 
    branchToString (depth+1) f body

| CondNode(thentree,elsetree) => 
  "ifthen " ^ treeToString (depth+1) f thentree ^ 
  Pretty.newline depth ^ "else " ^ treeToString (depth+1) f elsetree

| CaseConNode(cases,default) => 
  "caseof " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "| ") 
  (fn (i,tree) => Int.toString i ^ ":" ^ treeToString (depth+1) f tree)
  (IMap.listItemsi cases) ^ 
  (Pretty.newline depth ^ "| _ => " ^ treeToString (depth+1) f default)

| CaseSConNode(cases,default) => 
  "scase of " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "| ")
  (fn (c,tree) => Constants.constant_toString c ^ ":" ^ 
    treeToString (depth+1) f tree)
  (CMap.listItemsi cases) ^ 
  (Pretty.newline depth ^ "| _ => " ^ treeToString (depth+1) f default)

| CaseExConNode(cases,default) =>
  "excase of " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "| ")
  (fn (e,tree) => "?:" ^ treeToString (depth+1) f tree)
  (MILTy.Map.listItemsi cases) ^ 
  (Pretty.newline depth ^ "| _ => " ^ treeToString (depth+1) f default)

| Empty => ""

local
fun find' ((a,_), []) = a
  | find' ((a,br), path) =

    case (br,path) of
      (LetFunNode(funs,body), LetFun f :: path) =>
      (case Var.Map.find(funs, f) of
        NONE => []
      | SOME tree => find' (tree, path)
      )
    | (LetFunNode(funs,body), path) => find' (([],body), path)

    | (LetClassNode(meths, _), LetClass f :: path) =>
      (case Var.Map.find(meths, f) of
        NONE => []
      | SOME tree => find' (tree, path)
      )
    | (LetClassNode(_, body), path) => find' (([],body), path)

    | (CondNode(thentree, elsetree), Cond b :: path) => 
      if b 
      then find' (thentree, path)
      else find' (elsetree, path)

    | (CaseConNode(cases, default), CaseCon iopt :: path) =>
      (case iopt of
        NONE => find' (default, path)
      | SOME i =>
        case IMap.find(cases, i) of
          NONE => []
        | SOME tree => find' (tree, path)
      )

    | (CaseSConNode(cases, default), CaseSCon copt :: path) =>
      (case copt of
        NONE => find' (default, path)
      | SOME c =>
        case CMap.find(cases, c) of
          NONE => []
        | SOME tree => find' (tree, path)
      )

    | (CaseExConNode(cases, default), CaseExCon eopt :: path) =>
      (case eopt of
        NONE => find' (default, path)
      | SOME e =>
        case MILTy.Map.find(cases, e) of
          NONE => []
        | SOME tree => find' (tree, path)
      )

    | _ => []
in
  fun find (tree, path) = find' (tree, rev path)
end
  

fun insert (tree, patharg, item) =
let
  fun create (a,[]) = 
      (item :: a, Empty)

    | create (a, LetFun f :: path) = 
      (a, LetFunNode(Var.Map.insert(Var.Map.empty, f, create' path), Empty))

    | create (a, LetClass f :: path) =
      (a, LetClassNode(Var.Map.insert(Var.Map.empty, f, create' path), Empty))

    | create (a, Cond b :: path) =
      if b 
      then (a, CondNode(create' path, empty))
      else (a, CondNode(empty, create' path))

    | create (a, CaseCon NONE :: path) = 
      (a, CaseConNode(IMap.empty, create' path))
    | create (a, CaseCon (SOME i) :: path) = 
      (a, CaseConNode(IMap.insert(IMap.empty, i, create' path), empty))
     
    | create (a, CaseSCon NONE :: path) = 
      (a, CaseSConNode(CMap.empty, create' path))
    | create (a, CaseSCon (SOME c) :: path) = 
      (a, CaseSConNode(CMap.insert(CMap.empty, c, create' path), empty))
     
    | create (a, CaseExCon NONE :: path) = 
      (a, CaseExConNode(MILTy.Map.empty, create' path))
    | create (a, CaseExCon (SOME e) :: path) = 
      (a, CaseExConNode(MILTy.Map.insert(MILTy.Map.empty, e, create' path), 
        empty))


  and create' path = create ([], path)
     
  fun insert' ((a,br), []) = (item::a, br)
    | insert' ((a,Empty), path) = create (a,path)
    | insert' ((a,br), path) = 
      let 
      fun insertBr (br,path) =
      case (br,path) of

      (Empty, path) =>
      #2 (create' path)

    | (LetFunNode(funs, body), LetFun f :: path) =>
      (case Var.Map.find(funs, f) of
        NONE => 
        LetFunNode(Var.Map.insert(funs, f, create' path), body)

      | SOME tree => 
        LetFunNode(Var.Map.insert(funs, f, insert' (tree, path)), body)
      )

    | (LetFunNode(funs, body), path) =>
      LetFunNode(funs, insertBr(body,path))

    | (_, LetFun f :: path) =>
      LetFunNode(Var.Map.insert(Var.Map.empty, f, create' path), br)

    | (LetClassNode(meths, body), LetClass f :: path) =>
      (case Var.Map.find(meths, f) of
        NONE => 
        LetClassNode(Var.Map.insert(meths, f, create' path), body)

      | SOME tree => 
        LetClassNode(Var.Map.insert(meths, f, insert' (tree, path)), body)
      )

    | (LetClassNode(meths, body), path) =>
      LetClassNode(meths, insertBr(body,path))

    | (_, LetClass f :: path) =>
      LetClassNode(Var.Map.insert(Var.Map.empty, f, create' path), br)

    | (CondNode(thentree, elsetree), Cond b :: path) =>
      if b 
      then CondNode(insert' (thentree, path), elsetree)
      else CondNode(thentree, insert' (elsetree, path))

    | (CaseConNode(cases, default), CaseCon iopt :: path) =>
      (case iopt of
        NONE => CaseConNode(cases, insert' (default, path))
      | SOME i =>
        case IMap.find(cases, i) of
          NONE => CaseConNode(IMap.insert(cases, i, create' path), default)
        | SOME tree => 
          CaseConNode(IMap.insert(cases, i, insert' (tree, path)), default)
      )

    | (CaseSConNode(cases, default), CaseSCon copt :: path) =>
      (case copt of
        NONE => CaseSConNode(cases, insert' (default, path))
      | SOME c =>
        case CMap.find(cases, c) of
          NONE => CaseSConNode(CMap.insert(cases, c, create' path), default)
        | SOME tree => 
          CaseSConNode(CMap.insert(cases, c, insert' (tree, path)), default)
      )

    | (CaseExConNode(cases, default), CaseExCon eopt :: path) =>
      (case eopt of
        NONE => CaseExConNode(cases, insert' (default, path))
      | SOME e =>
        case MILTy.Map.find(cases, e) of
          NONE => 
          CaseExConNode(MILTy.Map.insert(cases, e, create' path), default)
        | SOME tree => 
          CaseExConNode(MILTy.Map.insert(cases, e, insert'(tree,path)),default)
      ) 


     | (br, path) =>
      Debug.fail ("MILPathOps.Map.insert: match failure with\ntree = " 
       ^ treeToString 0 (fn _ => "*") tree ^ "\npath = " ^ toString patharg)

   in (a,insertBr (br,path)) end


in
  insert' (tree, rev patharg)
end

fun toString f = treeToString 0 f

end (* of structure Map *)
  


end (* of local open *)

end (* of struct *)

