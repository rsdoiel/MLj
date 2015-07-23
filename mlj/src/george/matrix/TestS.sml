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

functor TestS(A:MATRIX)=
struct
   (* TestS.test is for testing the invertonevec function of MATRIX structures. It uses
      the AT&T Rand struct for generating random numbers. *)

   type seed=real (* used as a seed by the AT&T random number generator *)

   fun test
     {seed:real, (* the matrix used should be a function of this *)
      nx:int,
      ny:int,
      diagadd:real
          (* if diagadd<>0, diagadd * the sum of the absolute values of the entries of
             a row will be added to the diagonal element on that row (so if abs(diagadd)>1 the
             matrix should not be singular). *)
      }:real Vector.vector=
   let
      val randfun=Rand.mkRandom seed
      fun randnz ()= (* Random non-zero.  Hopefully this will be in [-1.0,1.0] *)
         (2.0*Rand.norm(randfun())-1.0)
      val size=nx*ny
      val mat=A.create(size,size)
      exception Finished
      fun next(x,y)=if x=0 then if y=0 then raise Finished else
         (nx-1,y-1) else (x-1,y)
      fun num(x,y)=x+y*nx
      fun up(x,y) =(x,(y+1) mod ny)
      fun down(x,y)=(x,(y-1) mod ny)
      fun left(x,y)=((x-1) mod nx,y)
      fun right(x,y)=((x+1) mod nx,y)

      fun randize(x,y)=
      let
         val ns as [nu,nd,nl,nr]=[randnz(),randnz(),randnz(),randnz()]
         val mid=diagadd*(List.foldl op+ 0.0 (List.map Real.abs ns))
         val p=num(x,y)
         val pu=num(up(x,y))
         val pd=num(down(x,y))
         val pl=num(left(x,y))
         val pr=num(right(x,y))
         val _=
           (A.update(mat,p,p,mid);
            A.update(mat,p,pu,nu);
            A.update(mat,p,pd,nd);
            A.update(mat,p,pl,nl);
            A.update(mat,p,pr,nr)
            )
      in randize(next(x,y))
      end
      val _= randize(nx-1,ny-1) handle Finished => {}

      val inverted=A.invertonevec(mat)
      val result=Vector.tabulate(size,fn i=>A.vector_sub(inverted,i))
   in
      result
   end
end
