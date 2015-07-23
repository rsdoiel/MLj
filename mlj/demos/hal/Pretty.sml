structure Pretty :> Pretty =
  struct
  (*Printing items: compound phrases, strings, and breaks*)
  datatype t = 
      Block of t list * int * int 	(*indentation, length*)
    | String of string
    | Break of int;			(*length*)

  (*Add the lengths of the expressions until the next Break; if no Break then
    include "after", to account for text following this block. *)
  fun breakdist (Block(_,_,len)::es, after) = len + breakdist(es, after)
    | breakdist (String s :: es, after) = size s + breakdist (es, after)
    | breakdist (Break _ :: es, after) = 0
    | breakdist ([], after) = after;

  fun pr (e, margin) =
   let val space = ref margin

    (*   fun blanks n = (Schan.write(StringCvt.padLeft #" " n "");  
		       space := !space - n)
    *)
   fun blanks n = let fun nb 0 = ""
                        | nb n = " " ^ (nb (n-1))
                  in
                      (Schan.write(nb n);  
		       space := !space - n)
                  end

       fun newline () = (Schan.write("\n");  space := margin)

       fun printing ([], _, _) = ()
	 | printing (e::es, blockspace, after) =
	  (case e of
	       Block(bes,indent,len) =>
		  printing(bes, !space-indent, breakdist(es,after))
	     | String s => (Schan.write(s);   space := !space - size s)
	     | Break len => 
		 if len + breakdist(es,after) <= !space 
		 then blanks len
		 else (newline();  blanks(margin-blockspace));
	    printing (es, blockspace, after))
   in  printing([e], margin, 0);  newline()  end;

  fun length (Block(_,_,len)) = len
    | length (String s) = size s
    | length (Break len) = len;

  val str = String  and  brk = Break;

  fun blo (indent,es) =
    let fun sum([], k) = k
	  | sum(e::es, k) = sum(es, length e + k)
    in  Block(es,indent, sum(es,0))  end;
  end;
