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

functor Test(A:MATRIX)=
struct
   (* Test.test is for testing the invertonevec function of MATRIX structures. It uses
      the AT&T Rand struct for generating random numbers. *)

   type seed=real (* used as a seed by the AT&T random number generator *)

   fun test
     {seed:real, (* the matrix used should be a function of this *)
      size:int, (* number of dimensions of the matrix *)
      nzeros:int, (* number of non-zeros in a row *)
      diagadd:real
          (* if diagadd<>0, diagadd * the sum of the absolute values of the entries of
             a row will be added to the diagonal element on that row (so if abs(diagadd)>1 the
             matrix should not be singular). *)
      }:real Vector.vector=
   let
      val randfun=Rand.mkRandom seed
      fun randcol ()=Rand.range(0,size-1)(randfun())
      fun randnz ()= (* Random non-zero.  Hopefully this will be in [-1.0,1.0] *)
         (2.0*Rand.norm(randfun())-1.0)

      val mat=A.create(size,size)
      fun randrow(rnum,todo,sumsofar)=
         if todo=0 then
            if Real.==(diagadd,0.0)
            then
               {}
            else
               A.update(mat,rnum,rnum,diagadd*sumsofar)
         else
            let
               val col=randcol()
               val ent=randnz()
               val _=A.update(mat,rnum,col,ent)
            in
               randrow(rnum,todo-1,sumsofar+Real.abs ent)
            end
      fun randmat(nextrow)=if nextrow<0 then {} else
         (randrow(nextrow,nzeros,0.0);
          randmat(nextrow-1))

      val _=randmat(size-1)

      val inverted=A.invertonevec(mat)
      val result=Vector.tabulate(size,fn i=>A.vector_sub(inverted,i))
   in
      result
   end
end
