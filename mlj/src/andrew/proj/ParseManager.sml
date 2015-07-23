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
(* Parse manager: obtain source files from SourceManager, parse them    *)
(* and check syntactic restrictions.                                    *) 
(*======================================================================*)
structure ParseManager :> PARSEMANAGER =
struct

(*----------------------------------------------------------------------*)
(* Cached parse trees in large and small form, with the timestamped ref *)
(* of the file from which they were derived.                            *)
(* Possible future `optimisation': to trade space against time, only    *)
(* keep large parse trees of most recently accessed files.              *)
(* The fileref not stored if this entity has been `frozen'.             *)
(*----------------------------------------------------------------------*)
type Info = 
  (Syntax.Dec * SmallSyntax.Dec * SourceMap.sourcemap) * Entity.FileRef option

type Cache = Info Entity.Map.map

datatype Result =
  Unchanged
| NotFound
| Fail
| Success of Info

(*----------------------------------------------------------------------*)
(* The source cache							*)
(*----------------------------------------------------------------------*)
val cache = ref (Entity.Map.empty : Cache)

fun kill p = 
  cache := Entity.Map.filteri (fn (entity,_) => not (p entity)) (!cache)
  

fun freeze p = 
let
  val cache' = Entity.Map.filteri (fn (entity,_) => p entity) (!cache)
in
  cache := Entity.Map.map 
  (fn ((_,topexp,sm),filerefopt) => (([],topexp,sm), NONE))
    cache'
end

fun freezeAll () = freeze (fn _ => true)

(*----------------------------------------------------------------------*)
(* Hack to add "local open Basis"					*)
(*----------------------------------------------------------------------*)
fun addBasis topbind =
case topbind of
  [(loc,Syntax.Structure [(strid,strexp as (loc',_),siginfo)])] =>
  [(loc,Syntax.Structure [(strid,   
     (loc',Syntax.StrLet([(loc', 
      Syntax.Open [[Symbol.symbol (JavaString.fromString "Basis")]])], 
        strexp)), siginfo)])]

| other => other

(*----------------------------------------------------------------------*)
(* Do the parse and update the cache.					*)
(*----------------------------------------------------------------------*)
fun doparse (entity, targetref) =
  case SourceManager.load (entity, targetref) of
    SourceManager.Changed(s, isbasis, sourceref) =>
    PrintManager.process ("Parsing " ^ EntityOps.descriptionWithFile 
      (entity, valOf sourceref), true)
    (fn () =>
      let
        val { AST, errors, sourcemap } = Parse.parse_string (s, not isbasis)
      in
        PrintManager.printErrors (sourcemap, errors);
        if List.exists Error.isSerious errors
        then Fail
        else         
          PrintManager.process ("Checking", false)
          (fn () =>
          let
            val { AST, errors } = 
            SyntaxCheck.check 
            { entity = entity, AST = valOf AST, sourcemap = sourcemap }
          in
            PrintManager.printErrors (sourcemap,errors); 
            if List.exists Error.isSerious errors 
            then Fail
            else
            let
              val topbind = if isbasis then AST else addBasis AST
              val smallexp = SyntaxConvert.convert topbind
              val result = (topbind, smallexp, sourcemap)
            in
              cache := Entity.Map.insert(!cache, entity, (result,sourceref));
              Success (result, sourceref)
            end
          end)
      end)

  | SourceManager.Unchanged =>
    Unchanged

  | SourceManager.Failed => 
    NotFound

(*----------------------------------------------------------------------*)
(* parse(entity, NONE) parses the file corresponding to the entity	*)
(*   given, returning Changed(r, time) if successful or Failed if not.  *)
(* parse(entity, SOME time) parses the file corresponding to the        *)
(*   entity given, if its timestamp is more recent than time. If it is  *)
(*   not more recent then Unchanged is returned.                        *)
(*----------------------------------------------------------------------*)
fun parse (entity as (etype,id), targetfilerefopt) =
  case Entity.Map.find(!cache, entity) of

    (* If it's in the cache then only re-parse if it's out of date *)
    SOME (result, cachefilerefopt) =>
    (case cachefilerefopt of
      NONE => 
      Success(result, cachefilerefopt)

    | SOME cachefileref =>
      let
        val reparseresult = doparse (entity, cachefilerefopt)
      in
      case targetfilerefopt of
        NONE => 
        (case reparseresult of
          Unchanged =>
          Success(result, cachefilerefopt)
        | other => other)

      | SOME targetfileref =>
        case reparseresult of
          Unchanged =>
          if EntityOps.>(cachefileref, targetfileref)
          then Success(result, cachefilerefopt)
          else Unchanged

        | other => other
      end)
      
    (* Otherwise always re-parse *)
  | NONE =>
    doparse (entity, targetfilerefopt)

end (* of struct *)

