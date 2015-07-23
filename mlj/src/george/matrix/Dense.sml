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

(* Dense:MATRIX implements some matrix operations needed for the Basic
   Block code (though it is intended that it may be recycled later).

   Matrices are effectively two-dimensional arrays of reals.  We only
   implement dense matrices.  Rows and columns are numbered from 0;
   the row number is always first.
   *)

structure Dense:>MATRIX=
struct
   type elem=real
   type vector=RealArray.realarray
   val vector_sub=RealArray.sub

   type matrix=vector array (* Each vector contains one row *)

   fun matrix_sub(m,i,j)=RealArray.sub(Array.sub(m,i),j)

   exception Dimensions

   fun create(m,n)=
      if m<=0 orelse n<=0 then raise Dimensions
      else
          Array.tabulate(m,fn _=> RealArray.array(n,0.0))

   fun update(m,i,j,r)=
   (let
      val row=Array.sub(m,i)
      val _= RealArray.update(row,j,r+RealArray.sub(row,j))
   in
      {}
   end handle Subscript => raise Dimensions)

   fun ncols m=RealArray.length(Array.sub(m,0))
   fun nrows m=Array.length m

   (* We now come to the functions required for invertvec.  Some of these
      may later be made public *)
   fun dotprod(m,j,v)=
   (* dotprod computes the dot product of the vector v with
      column j of the matrix m *)
   let
      fun dp_to_0(sofar,i)=
         if i<0
         then sofar
         else
            let
               val mij=matrix_sub(m,i,j)
               val vj=RealArray.sub(v,j)
            in
               dp_to_0(sofar+mij*vj,i-1)
            end
   in
      dp_to_0(0.0,RealArray.length v-1)
   end


   fun mulvec(m,v)=
   (* mulvec multiplies the matrix m by the vector v *)
      Vector.tabulate(ncols m,fn j=>dotprod(m,j,v))

   fun inverseuppermulvec(U,v)=
   (* U should be an upper triangular matrix.  This function returns U^{-1} v. *)
   let
      val n=nrows U
      val ans=RealArray.array(n,0.0) (* the answer will be put here *)
      fun sum(sofar,row,j)=(* sofar+sum for j' from j+1 to n-1 of  row_j * ans_j' *)
      let
         val j'=j+1
      in
         if j'>=n
         then sofar
         else sum(sofar+RealArray.sub(row,j')*RealArray.sub(ans,j'),row,j')
      end
      fun do_to_0 i=
      (* ans(j) is computed for j>i; compute the rest *)
         if i<0 then {}
         else
         let
            val s=sum(0.0,Array.sub(U,i),i)
            val quot=matrix_sub(U,i,i)
            val _=RealArray.update(ans,i,(RealArray.sub(v,i)-s)/quot)
         in
            do_to_0 (i-1)
         end
      val _= do_to_0 (n-1)
   in
      ans
   end


   fun mulvec1 m= (* Return the left-hand column of m *)
   let (* There is no RealArray.tabulate right now, unfortunately *)
      val vec=RealArray.array(nrows m,0.0)
      val _ = List.app RealArray.update (List.tabulate(nrows m,fn i=>(vec,i,
         RealArray.sub(Array.sub(m,i),0))))
   in
      vec
   end

   fun swaprows(m,i,j)=
   (* swaprows interchanges rows i and j *)
   let
      val swap=Array.sub(m,i)
      val _=Array.update(m,i,Array.sub(m,j))
      val _=Array.update(m,j,swap)
   in
      ()
   end

   fun addrow(m,j,r,g)=
   (* addrow adds g*row r to row j. *)
   let
      val rowr=Array.sub(m,r)
      val rowj=Array.sub(m,j)
      fun ar_to_0 i=
         if i<0 then ()
         else
            let
               val mri=RealArray.sub(rowr,i)
               val mji=RealArray.sub(rowj,i)
               val _=RealArray.update(rowj,i,mji+g*mri)
            in
               ar_to_0(i-1)
            end
   in
      ar_to_0(ncols m -1)
   end

   fun identity n=
   (* returns the n*n identity matrix *)
   let
      val init=create(n,n)
      val _=List.app update (List.tabulate(n,fn i=>(init,i,i,1.0)))
   in
      init
   end

   exception IllConditioned

   fun invertonevec U=
   let
      (* The strategy is as follows.  Let A be the initial value of U.
         We create a new identity matrix K and maintain the invariant
         A=(K^{-1})U, turning U into an upper triangular matrix.  The
         answer is then K(U^{-1})e_1.  Since U is then upper
         triangular, computing (U^{-1})e_1 can be done just from its
         top left element (with a bit more work we could handle
         general vectors but there's no need).

         We transform (K,U) so that U is upper triangular by repeatedly
         performing row interchanges and row additions on both U and K
         (such row operations correspond to premultiplying by a matrix
         T; the invariant A=(K^{-1})U is maintained because
         (TK)^{-1} TU = KU).

         The pivot value is chosen to be the largest element of each column.
         *)
      val n=ncols U
      val _ =if n<>nrows U then raise Dimensions else {}

      val K=identity n
      fun do_to_n r=
         (* For j>i<r, U_ij should now be 0.  Complete the triangularisation *)
         if r>=n-1 then {}
         else
            let
               fun max_to_n(sofar,index,i)=
                  if i=n then index
                  else
                  let
                     val try=Real.abs(RealArray.sub(Array.sub(U,i),r))
                  in
                     if try<=sofar then max_to_n(sofar,index,i+1)
                     else
                        max_to_n(try,i,i+1)
                  end
               val k=max_to_n(~1.0,0,r) (* compute k>=r maximising
                                           abs(U_{rk}) *)
               (* Interchange row k and r if different *)
               val _=if k<>r then (swaprows(U,k,r);swaprows(K,k,r))
                             else {}

               val recip=1.0/RealArray.sub(Array.sub(U,r),r)
               val _= if Real.abs(recip)<=0.0 orelse not(Real.isNormal recip)
                      then raise IllConditioned
                      else {}

               fun make0_to_n i=
               (* Do row combinations to make the lower part of column
                     r 0. *)
                  if i=n then {}
                  else
                     let
                        val uir=RealArray.sub(Array.sub(U,i),r)

                        val _=if Real.compare(uir,0.0)<>EQUAL
                        then
                           (addrow(U,i,r,~uir*recip);
                            addrow(K,i,r,~uir*recip))
                        else {}
                     in
                        make0_to_n(i+1)
                     end
               val _= make0_to_n (r+1)
            in
               do_to_n (r+1)
            end

      val _=do_to_n 0
   in
      inverseuppermulvec(U,mulvec1 K)
   end
end
