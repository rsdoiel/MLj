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

(* RowMat:ROWMAT implements a sparse matrix as it is reduced by row reduction.  Operations are mostly
   done by side-effects. *)
structure RowMat:>ROWMAT=
struct
   type elem=real
   structure R=Real
   (* We do not mention reals from now on (in case we need Real128s or something). R should have a
      *+ field. *)

   type entry={row:int,column:int,value:elem ref}
   datatype rowmat=R of
     {rows:entry list Array.array, (* rows in increasing order of column number *)
      rowls:int Array.array, (* row lengths *)
      columns:entry list Array.array, (* columns in any order *)
      remaining_colls:int Array.array (* number of entries in each column not deleted by delrow *)
      }

   (* Construct a structure for sorting entries, rows first *)
   structure EntryKey:>ORD_KEY where type ord_key=entry =
   struct
       type ord_key=entry
       fun compare({row=row1,column=column1,...}:entry,{row=row2,column=column2,...}:entry)=
          (case Int.compare(row1,row2) of
             GREATER => GREATER
          |  LESS => LESS
          |  EQUAL => Int.compare(column1,column2)
          )
   end
   structure EntrySort=Sort(EntryKey)

   fun create (entries,n)=
   let
      (* sort entries *)
      val sorted_entries=EntrySort.sort entries
      (* replace duplicate entries by ones in which the values have been summed *)
      fun sumdup(sofar,ent,[])=ent::sofar
      |   sumdup(sofar,ent,ent2::rest)=
         if #row ent = #row ent2 andalso #column ent = #column ent2
         then sumdup(sofar,{row= #row ent,column= #column ent,value=ref(!(#value ent)+ !(#value ent2))},
            rest)
         else sumdup(ent::sofar,ent2,rest)
      val summed_entries= (* this list is in reverse order *)
         (case sorted_entries of
            [] => []
         |  hd::tl => sumdup([],hd,tl)
         )

      (* generate columns and remaining_colls first *)
      val columns=Array.array(n,[])
      val remaining_colls=Array.array(n,0)

      val _=
         List.app
            (fn ent as {row,column,value} =>
               let
                  val oldcol=Array.sub(columns,column)
                  val _=Array.update(columns,column,ent::oldcol)
                  val oldl=Array.sub(remaining_colls,column)
                  val _=Array.update(remaining_colls,column,oldl+1)
               in
                  {}
               end
               )
            summed_entries

      (* Now for rows *)
      val rows=Array.array(n,[])
      val rowls=Array.array(n,0)
      val _=
         List.app
      (* Here we are relying on summed_entries being reverse ordered by row and then column. *)
            (fn ent as {row,column,value} =>
               let
                  val oldrow=Array.sub(rows,row)
                  val _=Array.update(rows,row,ent::oldrow)
                  val oldl=Array.sub(rowls,row)
                  val _=Array.update(rowls,row,oldl+1)
               in
                  {}
               end
               )
            summed_entries
   in
      R{rows=rows,rowls=rowls,columns=columns,remaining_colls=remaining_colls}
   end

   fun getrow(R{rows,...},i)=Array.sub(rows,i)
   fun lrow(R{rowls,...},i)=Array.sub(rowls,i)
   fun getcol(R{columns,...},i)=Array.sub(columns,i)
   fun l_remaining_column(R{remaining_colls,...},i)=Array.sub(remaining_colls,i)

   fun delrow(mat as R{remaining_colls,...},i)=
      List.app
         (fn {column,...} =>
            let
               val oldl=Array.sub(remaining_colls,column)
            in
               Array.update(remaining_colls,column,oldl-1)
            end
            )
         (getrow(mat,i))

   fun delcol(mat as R{columns,...},i)=
      Array.update(columns,i,[])

   fun row_op(mat as R{rows,rowls,columns,remaining_colls},{pivot_row,pivot_col,target,multiplier})=
   let
      fun new_ent(ent as {row,column,value})=
      (* used when we have fillin to adjust the column arrays for the new entry *)
      let
         val oldcol=Array.sub(columns,column)
         val _=Array.update(columns,column,ent::oldcol)
         val oldl=Array.sub(remaining_colls,column)
         val _=Array.update(remaining_colls,column,oldl+1)
      in
         ent
      end

      fun row_mul(sofar,p)=
         (case p of
            [] => sofar
         |  {row,column,value}::rest =>
               row_mul(new_ent {row=target,column=column,
                  value=ref(R.*(!value,multiplier))}::sofar,rest)
         )

      fun row_com(sofar,p,t)=
         (case p of
            [] => List.revAppend(t,sofar)
         |  {column=columnp,value=valuep,...}::restp =>
            (case t of
               [] => row_mul(sofar,p)
            |  (tent as {column=columnt,value=valuet,...})::restt =>
               (case Int.compare(columnp,columnt) of
                  LESS =>
                  row_com(new_ent {row=target,column=columnp,value=ref(!valuep*multiplier)}::sofar,
                     restp,t)
               |  GREATER => row_com(tent::sofar,p,restt)
               |  EQUAL =>
                     if columnt=pivot_col
                     then (valuet:=0.0;row_com(sofar,restp,restt))
                     else (valuet:=R.*+(multiplier,!valuep,!valuet);row_com(tent::sofar,restp,restt))

               )
           )
        )

      val new_row=List.rev(row_com([],getrow(mat,pivot_row),getrow(mat,target)))
      val rowl=List.length(new_row)
      val _= Array.update(rows,target,new_row)
      val _= Array.update(rowls,target,rowl)
   in
      {}
   end
end
