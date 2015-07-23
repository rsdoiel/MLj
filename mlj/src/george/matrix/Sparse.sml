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

(* Sparse:MATRIX is an implementation of MATRIX that uses
   sparse matrix techniques.
   *)
structure Sparse:>MATRIX=
struct
(* A matrix is stored as its dimension plus a list of RowMat.entry's.  This means we don't actually do
   any work until we come to the invertonevec function itself *)
   type matrix=
     {size:int, (* all matrices are square *)
      entries:RowMat.entry list ref
      }

   type elem=real
   structure R=Real
   structure RA=RealArray

   val zero=R.fromInt(0)
   val one=R.fromInt(1)
   val nan=zero/zero
   (* this is an initial value for entries of resvec and should not actually be output;
      it can be replaced by any value. *)

(* We do not mention Reals from now on.  The standard arithmetic operators (R.+ etcetera)
   are assumed to exist, and also R.*+.  We also take absolute values, using R.abs, and compare them in
   pivot selection.  If we later want to adapt this code for some other field, the
   following steps are required.
   1) substitute appropriate R and RA structures here and in RowMat.
   2) if the field is discrete, simplify select_pivot, removing all comparisons and absolute value
      computations (in that case, we will not need any comparisons or
      the .isNormal, .max and .abs operations
      in R), since we will only be interested in minimising fill-in.
   *)

   type vector=RA.array

   val vector_sub=RA.sub
   (* raised if there is a problem with the number of dimensions *)

   exception Dimensions

   fun create(m,n)=
      if m<>n orelse n<1
      then raise Dimensions
      else {size=n,entries=ref []}

   fun update({size,entries},row,column,toadd)=
   let
      val _=
         if row<0 orelse row>=size orelse column<0 orelse column>=size
         then raise Dimensions
         else {}
      val _= entries:= {row=row,column=column,value=ref toadd}::(!entries)
   in
      {}
   end

   val nrows=3  (* nrows and maxrat are used in invertonevec to control pivot selection *)
   val maxrat=8.0

   exception IllConditioned

   fun invertonevec {size,entries}=
   let
      (* Strategy.  We need to find the result without using gross amounts of
         space or time for large matrices (with 300 dimensions plus).  But neither
         space nor time are so short that factors of 2 do not matter; we are unlikely
         to need to handle 100000s of dimensions.  So we use a fairly simple and general
         method, that hopefully I can adapt for other purposes later.

         This is as follows.  Let A be the matrix.  The general approach is to row-reduce
         A to PLUQ where P and Q are permutation matrices, L is lower triangular, and U is
         upper triangular, and then compute Q'U'L'P' x where ' means matrix inverse (for sparse matrices,
         it hardly ever pays to compute the inverse since multiplying the lower and upper triangular
         matrices produces too many non-zeros; there is no reason why the inverse should be sparse).
         However what we actually do is compute x"= PL'P' x and U"= PL'P' A = PUQ, and then find
         U"' x".

         First stage: finding x" and U".  We start off with M=A and reduce M by row operations to
         U".  Row operations correspond to premultiplication by a matrix, so by doing the same operations
         on x we find x".  We use Gaussian elimination for the row operations, choosing a sequence of
         pivots on M.

         To choose the pivots we use a Markowitz-style criterion as follows.  Using
         a priority queue (implemented by Priority:PRIORITY) we choose the nrows rows with fewest non-zero
         entries (increasing nrows will improve - slightly - the time and space performance of the
         rest of the algorithm at the expense of time taken to scan the nrows rows themselves).  We
         choose the pivot from these rows by the following criteria, in decreasing order of importance.
         1) maxrat * the absolute value of the pivot entry must be >= the maximum absolute value of
            any entry in the row.
         2) the product of the (number of non-zeros in the row - 1) and the (number of non-zeros in the
            column - 1) is minimised.
         3) the absolute value of the pivot entry is maximised.

         For further discussion see Osterby & Zlatev, "Direct Methods for Sparse Matrices", published by
         Springer as Lecture Notes in Computer Science, number 157.

         Improvements: according to O&Z we could reduce fill in and improve performance by deliberately
         ignoring very small non-zeros that would otherwise get added to the matrix, and refine the
         resulting erroneous answer using an iterative method.  All sorts of methods could be used to
         reduce the space taken up by the matrix.  If we get into
         really serious trouble it would probably be best to recode the whole thing in
         C or FORTRAN (this is non-trivial since one still needs linked lists
         and garbage collection) or use an off-the-shelf package.  Note that the space and time
         performance can be greatly improved if the matrix happens to be symmetric and positive
         definite.

         Subscript exceptions should not occur now (because we checked
         all array dimensions and entries in create and update) so except when this code
         is being debugged it should be safe to turn off bounds checking if such an option
         exists.
         *)

      val rmat=RowMat.create(!entries,size) (* rmat is M as it is reduced *)
      val xvec=RA.array(size,zero)
      val _= RA.update(xvec,0,one)
      (* xvec is the vector which is initally what we want to find A' of, and is reduced in step
         with M. *)

      val pqueue=Priority.empty ()
      (* pqueue will be a priority queue containing all rows which have yet to be pivoted on
         ordered by how many non-zeros they contain. *)
      local
         fun all_rows r =
         if r=size then {}
         else
            let
               val {id}=Priority.insert(pqueue,{key=RowMat.lrow(rmat,r)})
               val _ = if id<>r then raise Fail "This can't happen" else {}
            in
               all_rows (r+1)
            end
      in
         val _= all_rows 0
      end

      fun select_pivot ():RowMat.entry option=
      let
         (* select_pivot applies the Markowitz criteria as outlined about to the matrix rmat
            using the rows in pqueue and returns the entry for the pivot.  It returns NONE if
            we are finished because pqueue is empty. *)
         datatype BSF= (* BSF means "best so far". *)
            NO
         |  YES of RowMat.entry*real*int
         (* the real is the absolute value, the int the (row-1)*(column-1) product for
            the best entry so far *)

         fun try_row({key=nentries,id=row},best_so_far)=
         let
            (* computes BSF for row, if it's better than best_so_far *)
            val elist=RowMat.getrow(rmat,row)
            (* For each entry in the elist pair it with its absolute value *)
            val eabs=
               List.map
                  (fn entry => (entry,R.abs(!(#value entry))))
                  elist
            val maxabs=
               (* maximum absolute value *)
               List.foldl
                  (fn ((_,a),b)=>R.max(a,b))
                  0.0
                  eabs
            val _=
               if maxabs<=0.0 orelse not(R.isNormal maxabs)
               then raise IllConditioned
               else {}

            fun try_ent((entry,absval),best_so_far)=
            (* this actually implements the Markowitz test *)
               if maxrat*absval<maxabs
               then best_so_far
               else
               let
                  val new_prod=(nentries-1)*(RowMat.l_remaining_column(rmat,#column entry)-1)
                  fun newbsf()=YES(entry,absval,new_prod)
               in
                  (case best_so_far of
                     NO => newbsf()
                  |  YES(old_entry,old_absval,old_prod) =>
                        (case Int.compare(old_prod,new_prod) of
                           LESS => best_so_far
                        |  GREATER =>newbsf()
                        |  EQUAL =>
                              if absval>old_absval then newbsf() else best_so_far
                        )
                  )
               end (* of try_ent *)
         in
            List.foldl
               try_ent
                  best_so_far
                  eabs
         end (* of try_row *)
      in
         (case Priority.lowest(pqueue,nrows) of
            [] => NONE
         |  p =>
            let
               val YES(ent,_,_)=
                  List.foldl
                     try_row
                        NO
                        p
            in
               SOME ent
            end
         )
      end (* of select_pivot *)

      val pi=IntArray.array(size,~1)
      val sigma=IntArray.array(size,~1)
      (* pi[x]    will be r if at step r we pivoted on row    x, or ~1 if x has not yet been pivoted on.
         sigma[y] will be r if at step r we pivoted on column y, or ~1 if y has not yet been pivoted on.
         Thus pi and sigma can be regarded as storing the permutation matrices P
         and Q.
         *)

      fun do_reduction r=
      (* if there is still pivotting to be done, do it and repeat, otherwise exit. *)
      if r=size then {}
      else
         let
            val pivot=valOf(select_pivot())

(* for testing.  . *)
(*
            val _= print("\n Pivot "^Int.toString(#row pivot)^
               Int.toString(#column pivot))
*)
            (* update pi and sigma, and remove row from priority queue and from column counts *)
            val _= IntArray.update(pi,#row pivot,r)
            val _= IntArray.update(sigma,#column pivot,r)
            val _= Priority.remove(pqueue,{id= #row pivot})
            val _= RowMat.delrow(rmat,#row pivot)

            (* Now pivot on the column, using pi to discover which elements need to be zeroed.*)
            val pivot_val= !(#value pivot)
            val pivot_col= RowMat.getcol(rmat,#column pivot)
            (* delete the copy of this column *)
            val _= RowMat.delcol(rmat,#column pivot)
            val _=
               List.app
                  (fn entry =>
                     if IntArray.sub(pi,#row entry)<0
                     then
                     (* row has not yet been pivotted on *)
                        let
                           val multiplier= R.~(R./(!(#value entry),pivot_val))

                           (* perform the row operation on rmat and xvec *)
                           val _= RowMat.row_op(rmat,
                             {pivot_row= #row pivot,
                              pivot_col= #column pivot,
                              target= #row entry,
                              multiplier=multiplier
                              })
                           val _=
                              RA.update(xvec,#row entry,
                                 R.*+(multiplier,RA.sub(xvec,#row pivot),
                                    RA.sub(xvec,#row entry)))

                           (* update the priority queue *)
                           val _=
                              Priority.bump(pqueue,{id= #row entry,newkey=RowMat.lrow(rmat,#row entry)})
                        in
                           {}
                        end
                     else
                        {}
                     )
                  pivot_col
         in
            do_reduction(r+1)
         end (* of do_reduction *)

      val _= do_reduction 0

      (* Invert pi and sigma *)
      fun invert_perm(perm:IntArray.array)=
      let
         val arr=IntArray.array(IntArray.length perm,~1)
         val _=
            IntArray.appi
               (fn (index,el)=>IntArray.update(arr,el,index))
               (perm,0,NONE)
      in
         arr
      end

      val inversepi=invert_perm pi
      val inversesigma=invert_perm sigma

      (* for s>r rowmat should have no entry in row inversepi[s], column inversesigma[s].
         We compute resvec= (rowmat)^{-1} xvec
         *)

      (* Compute the result vector *)
      val resvec=RA.array(size,nan)

      fun find_res r=
      (* find resvec[inversesigma[r]] in decreasing order of r *)
      let
         val rowr=IntArray.sub(inversepi,r)
         val colr=IntArray.sub(inversesigma,r)
         val matrow=RowMat.getrow(rmat,rowr)
         fun get_diag({row,column,value}::tl)= if column=colr then !value else get_diag tl
         val diag=get_diag matrow
         val rsum=
            List.foldl
               (fn ({column,value,...},sum_so_far)=>
                  if column=colr
                  then sum_so_far (* ignore diagonal element *)
                  else R.*+(!value,RA.sub(resvec,column),sum_so_far)
                  )
               zero
               matrow
         val _=
            RA.update(resvec,colr,
               R./(R.-(
                     RA.sub(xvec,rowr),
                     rsum),
                  diag)
               )
      in
         if r=0
         then {}
         else find_res(r-1)
      end

      val _=find_res(size-1)
   in
      resvec
   end (* invertonevec *)
end (* Sparse *)
