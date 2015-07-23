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
(* Fixity datatype and operations.					*)
(*======================================================================*)
structure Fixity :> FIXITY =
struct

datatype Fixity = 
  Infix of int * bool
| Nonfix

type Env = Fixity Symbol.OrdMap.map

fun convert NONE = Nonfix
  | convert (SOME fixity) = fixity

(*----------------------------------------------------------------------*)
(* Look up the precedence of a variable identifier			*)
(*----------------------------------------------------------------------*)
fun lookup (env, x) = getOpt(Symbol.OrdMap.find(env, x), Nonfix)

(*----------------------------------------------------------------------*)
(* Update the fixity environment for a list of identifiers		*)
(*----------------------------------------------------------------------*)
fun updateEnv (env, ids, Nonfix) =
    let
      fun remove ([], env) = env
        | remove (id::ids, env) = remove (ids,
          if isSome (Symbol.OrdMap.find(env,id)) 
          then #1 (Symbol.OrdMap.remove(env,id)) 
          else env)
    in
      remove (ids, env)
    end

  | updateEnv (env, ids, info) =
    let
      fun update ([], env) = env
        | update (id::ids, env) = 
          update (ids, Symbol.OrdMap.insert(env, id, info))
    in
      update (ids, env)
    end


(*----------------------------------------------------------------------*)
(* The initial fixity environment					*)
(*----------------------------------------------------------------------*)
val initialEnv =
foldr (fn ((v,p),m) => 
  Symbol.OrdMap.insert(m,Symbol.symbol (JavaString.fromString v),Infix p))
Symbol.OrdMap.empty
[
  ("div", (7, false)),
  ("mod", (7, false)),
  ("/",   (7, false)),
  ("*",   (7, false)),
  ("+",   (6, false)),
  ("-",   (6, false)),
  ("^",   (6, false)),
  ("::",  (5, true)),
  ("@",   (5, true)),
  ("<>",  (4, false)),
  ("=",   (4, false)),
  ("<",   (4, false)),
  (">",   (4, false)),
  ("<=",  (4, false)),
  (">=",  (4, false)),
  (":=",  (3, false)),
  ("o",   (3, false)),
  ("before", (0, false))
]

end


