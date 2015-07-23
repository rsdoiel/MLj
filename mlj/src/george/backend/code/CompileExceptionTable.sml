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

structure CompileExceptionTable:>COMPILEEXCEPTIONTABLE=
struct
   local
      open Code
      open AllPools
      open Labels
      open TryCatch
   
      datatype internal_exception_spec=I of
                                {start_lab:label,
				 end_lab:label,
				 handler_lab:label,
				 catch_type_handle:hndl option,
                                 priority:int}

      type t=internal_exception_spec

      (* We now declare sort_exceptions, which takes a list of 
         internal_exception_specs, the labels for which have
         been resolved, and outputs a list of exceptions to be written to the
         class file in that order such that if an exception is thrown 
         it is caught by the innermost enclosing catch clause with matching class
         and (if there are two catch clauses with the same range with matching
         classes) by ones of those with highest priority (it is undefined which 
         one, if there is more than one).  The effect of catch clauses which
         overlap is undefined.  
         *) 

      structure LenPriKey=
      struct
         type ord_key=t
         fun compare(
         (* comparison function so that high lengths, (or if those are equal)
            low priorities come first *)
            I{start_lab=s1,end_lab=e1,priority=p1,...},
            I{start_lab=s2,end_lab=e2,priority=p2,...})=
            (case Int.compare
               (Labels.index e2-Labels.index s2,
                Labels.index e1-Labels.index s1) of
               LESS => LESS
            |  GREATER => GREATER
            |  EQUAL => Int.compare(p1,p2)
            )
      end
      structure LenPriSort=Sort(LenPriKey)
      
      structure StartKey=
      struct
         type ord_key=t
         fun compare(
         (* Comparison function by start *)
            I{start_lab=s1,...},
            I{start_lab=s2,...})=
            Int.compare(Labels.index s1,Labels.index s2)
      end
      structure StartSort=Sort(StartKey)
   
      fun sort_exceptions elist=
      let
         (* Strategy (this is O(n^2) when it could be O(n log n) but currently
            I don't care).  Sort the exceptions in decreasing order of
            1) length 2) (where these are equal) priority.  
            Repeatedly: divide the contents of this list into two lists,
            list B (sorted) containing those exceptions whose ranges are either
            contained in or equal to but with higher priority than an earlier
            exception, and A (unsorted) the rest.  A should be a non-empty
            set of non-overlapping exceptions with minimal priority.  
            Sort A by position.  Amalgamate catch
            clauses in A where possible and remove null ones; prepend the result
            to the list to be output.  If B is non-empty, repeat the process with
            it. *)

         fun is_not_null(I{start_lab,end_lab,...})=
            Labels.index start_lab<Labels.index end_lab
            
         val non_null_elist=List.filter is_not_null elist
         val selist=LenPriSort.sort non_null_elist
   
         fun one_step(output_list,[])=List.concat(output_list)
         |   one_step(output_list,selist)=
         let
            (* compute A list and B list *)
            fun covers(I{start_lab=s1,end_lab=e1,...},
                       I{start_lab=s2,end_lab=e2,...})=
            (* I1=[s1,e1); I2=[s2,e2).  True if I1 contains I2. *)
               Labels.index s1<=Labels.index s2 andalso 
               Labels.index e1>=Labels.index e2   
            val (Alist,Blistrev)=
               List.foldl
                  (fn (ex,(Alist_sf,Blistrev_sf))=>
                     if List.exists
                        (fn aitem =>
                           covers(aitem,ex))
                        Alist_sf
                     then
                        (Alist_sf,ex::Blistrev_sf)
                     else
                        (ex::Alist_sf,Blistrev_sf)
                  )     
                  ([],[])
                  selist
            val Blist=List.rev(Blistrev)
   
            fun simplify(done,last_ex,[])=last_ex::done
            |   simplify(done,ex1,ex2::rest)=
            let
                val I{start_lab=s1,end_lab=e1,catch_type_handle=c1,handler_lab=h1,...}=
                   ex1
                val I{start_lab=s2,end_lab=e2,catch_type_handle=c2,handler_lab=h2,...}=
                   ex2
                val do_merge=
                (* handlers are equal *)
                   (Labels.index h1=Labels.index h2)
                andalso
                (* Intervals overlap *)
                   Labels.index s2 <= Labels.index e1
                andalso
                (* classes are equal *)
                   (case (c1,c2) of
                      (NONE,NONE) => true
                   |  (SOME ch1,SOME ch2)=> ch1()=ch2()
                   |  _ => false
                   )
            in
               if do_merge 
               then simplify(done,
                  I{start_lab=s1,end_lab=e2,catch_type_handle=c1,handler_lab=h1,
                    priority=0},rest)
               else simplify(ex1::done,ex2,rest)
            end
            val Ahd::Atl=StartSort.sort Alist
            val next_output=simplify([],Ahd,Atl)
         in
            one_step(next_output::output_list,Blist)
         end (* of one_step *)
      in
         one_step([],selist)
      end (* of sort_exceptions *)    
   in
      type t2=internal_exception_spec list

      fun pool_pass(A,l)=
(* A is the constant pool, l the TryCatch.t list *)
         List.map
           (fn ex=>
               I {start_lab=start_lab ex,
                  end_lab=end_lab ex,
                  handler_lab=handler_lab ex,
                  catch_type_handle=(
                     case catch_type ex of
                        SOME c => SOME(r_class(A,c))
                     |  NONE   => NONE
                     ),
                  priority=priority ex
                  })
           l

      val h2zero=Word8Vector.fromList[0w0,0w0]

      fun bytecode_pass l=
      let (* l is the result of pool_pass *)
         open Numbers
      (* optional_h2 is like h2 but takes an option and uses 0 if
         the option is NONE *)
         fun optional_h2(SOME h)=h2 h
         |   optional_h2(NONE)=h2zero
         val excep_list=sort_exceptions l
      in
         W8.combine
           (List.map
               (fn I{start_lab,end_lab,handler_lab,catch_type_handle,...} =>
                  W8.fromvList
                    [Labels.l2 start_lab,
                     Labels.l2 end_lab,
                     Labels.l2 handler_lab,
                     optional_h2(catch_type_handle)])
               excep_list
           )
      end
   end (* local *)
end (* struct *)







