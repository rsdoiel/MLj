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

(* Peephole:>PEEPHOLE performs various local optimisations on the
   bytecode for basic blocks.
   *)
structure Peephole:>PEEPHOLE=
struct
   val int_t=Types.F(0,Types.INT)

   fun peephole ilist=
   let
      val irev=List.rev ilist
      fun ph(sf,[])=sf
      |   ph(sf,l as h::t)=
      let
         fun nogo()=ph(h::sf,t)
      in
         (case l of
            Code.store(_,i1) ::
            Code.add _ ::
            Code.push c :: (* The INT forces the type of everything else *)
            Code.load(_,i2) :: rest =>
               if i1=i2 
               then
                  (case Constants.toInt c of
                     SOME ji =>
                        if JavaInt.isji2 ji
                        then
                           ph(Code.iinc(i1,ji) :: sf,rest)
                        else
                           nogo()
                  |  _ => nogo()
                  )     
               else nogo()
        (* While we're at it, do add push int where int is in [~5,~2] goes to
           sub (negation). *)
         |  Code.add _ ::
            Code.push c :: rest =>
               (case Constants.toInt c of
                  SOME ji =>
                  let
                     val nc=Constants.INT(JavaInt.numops.neg ji)
                  in
                     if Constants.cost nc < Constants.cost c
                     then
                        ph(Code.push nc::Code.sub int_t::sf,rest)
                     else
                        nogo()
                  end
               |  NONE => nogo()
               )
         |  _ => nogo()
         )
      end
   in
      ph([],irev)
   end
end
