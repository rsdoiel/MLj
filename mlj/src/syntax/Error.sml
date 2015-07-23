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
(* General error handling stuff						*)
(*======================================================================*)
structure Error :> ERROR = 
struct

datatype Degree = 
  Warning 
| Serious

datatype Error = 
  Error of Degree * Syntax.Location * string

fun error   (loc, message) = Error(Serious, loc, message)
fun warning (loc, message) = Error(Warning, loc, message)

fun append (Error(degree,loc,str), str') = Error(degree,loc,str ^ str')

fun isSerious (Error (Serious,_,_)) = true
  | isSerious _ = false

(*----------------------------------------------------------------------*)
(* If there are no errors or warnings, do nothing, otherwise print out  *)
(* the list in location order.                                          *)
(*----------------------------------------------------------------------*)

fun print(do_print:string->unit,s:SourceMap.sourcemap,errors:Error list) = 
let
   fun print_fn(Error(degree,loc,mess))=
      do_print(
         String.concat[
            (case degree of Warning => "Warning " | Serious => "Error "),
            "at ",
            PrintLoc.location2string(s,loc),
            ":",
            mess,
            "\n"
            ])

   (* Sort the errors by left location.  We delete all messages
      before the first ERROR in a list. *)
   val (_,numbered_errors)=
      List.foldl
         (fn(e,(n,sf)) =>
            (n+1,(e,n)::sf))
         (0,[])
         errors
   
   val sorted_errors=
      ListMergeSort.sort
         (fn((Error(_,x,_),nx),(Error(_,y,_),ny)) =>
            (case Int.compare(#left x,#left y) of
               LESS => false
            |  GREATER => true
            |  EQUAL => (nx<ny)
            ))
            numbered_errors
   
   val final_errors=
   let
      fun drop_lx(l,[])=[]
      |   drop_lx(l,list as ((Error(_,x,_),_)::rest))=
             if #left x = l 
             then 
                drop_lx(l,rest)
             else
                list 
   
      fun acc_errors(sf,[])=sf
      |   acc_errors(sf,(e as Error(Warning,_,_),_)::rest)=
             acc_errors(e::sf,rest)
      |   acc_errors(sf,(e as Error(Serious,x,_),_)::rest)=
             acc_errors(e::sf,drop_lx(#left x,rest))
   in
      List.rev(acc_errors([],sorted_errors))
   end
in
   List.app print_fn final_errors
end
end
