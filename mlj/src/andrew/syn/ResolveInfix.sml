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
(* Resolve infixes, functorised for use on expressions *and* patterns.	*)
(* Based on ML Kit code.                                                *)
(*======================================================================*)
functor ResolveInfix
(
  type Object

  (* Construct a two-element tuple *)
  val pair: Object * Object -> Object

  val asId: Object -> Syntax.symbol option
  val applyId: Syntax.symbol * Object -> Object
  val applyObj: Object * Object -> Object
) :
sig
  val resolve : Fixity.Env * Object list -> Object
  exception InfixError of string
end
=

struct

    exception InfixError of string

   (* We can stack "operators" (infixes and the implicit applications). *)

    datatype StackEntry = INFIXentry of Syntax.symbol * int
			| INFIXRentry of Syntax.symbol * int
			| APPLentry

   (* Keep track of the last thing we passed - needed to spot
      applications (two operands successively, with no intervening
      operator). *)

    datatype LastObj = ARG | OPER | VOID

   (* apply - apply a stack entry (infix(r) or appl) to the list of
      generated arguments. Might fail on source input like "A ::" where we
      run out of arguments while flushing the stack, so we make apply
      bulletproof. Note that the args list is in reverse order. *)

    fun apply(entry, (snd :: fst :: rest): Object list) =
          let
	    val thePair = pair(fst, snd)
	  in
	    (case entry of
	      INFIXentry(id, _) => applyId(id, thePair)
	    | INFIXRentry(id, _) => applyId(id, thePair)
	    | APPLentry => applyObj(fst, snd)
	    ) :: rest
	  end

      | apply(entry, _) =
	  raise InfixError
	        ("Give "
		 ^ (case entry of
		      INFIXentry(id, _) => 
  "the infix `" ^ Pretty.idToString id ^ "' more arguments."
		    | INFIXRentry(id, _) => 
  "the infixr `" ^ Pretty.idToString id ^ "' more arguments."
		    | APPLentry => Debug.fail "ResolveInfix.apply"))

    (*assocLeft(op1, op2) - precedence comparison of infix(r) and appl
     stack entries.  Application is the highest priority, and associates
     to the left. Other operators associate according to precedence
     (sec. 2.6, Definition of sml'96): If the precedences are not equal,
     association is to the side with the highest precedence, otherwise,
     the operators must associate to the same side according to their
     infix/infixr status (if they do not, an error message is given).*)

    and assocLeft (APPLentry, _) = true	(* APPL is highest (left) precedence. *)
      | assocLeft (_, APPLentry) = false
      | assocLeft (op1, op2) =
          precedence op1 > precedence op2 orelse
	  precedence op1 = precedence op2 andalso both_associate_left (op1, op2)

    and precedence (INFIXRentry(id,i)) = i
      | precedence (INFIXentry(id,i)) = i
      | precedence _  = Debug.fail "ResolveInfix.precedence"

    and op_as_string (INFIXentry(id,prec)) = Pretty.idToString id
      | op_as_string (INFIXRentry(id,prec)) = Pretty.idToString id
      | op_as_string _ = Debug.fail "ResolveInfix.op_as_string"

    and side (INFIXentry _) = "left"
      | side (INFIXRentry _) = "right"
      | side _ = Debug.fail "ResolveInfix.side"

    (*both_associate_left (op1,op2): op1 and op2 must have the same precedence*)
    and both_associate_left (INFIXentry(id1,prec1),INFIXentry(id2,prec2)) = true
      | both_associate_left (INFIXRentry(id1,prec1),INFIXRentry(id2,prec2)) = false
      | both_associate_left (op1,op2) =
          raise InfixError
	    ("Insert parentheses.  `"
	     ^ op_as_string op1 ^ "' and `" ^ op_as_string op2
	     ^ "' have the same precedence\nbut associate "
	     ^ side op1 ^ " and " ^ side op2 ^ ", respectively.")

   (* flushHigher - flush out all entries in the stack with higher
      effective precedence than "entry". Take INFIX, INFIXR, APPL status
      into account. *)

    fun flushHigher(entry, stack, output) =
      case stack
	of nil => (nil, output)
	 | top :: rest =>
	     if assocLeft(top, entry) then
	       flushHigher(entry, rest, apply(top, output))
	     else
	       (stack, output)


   (* flushAll - clear out all the stacked operators at the end of an
      atexp sequence. *)

    fun flushAll ([],          [item]) = item
      | flushAll ([],          output) = Debug.fail "ResolveInfix.flushAll"
      | flushAll (top :: rest, output) = flushAll (rest, apply (top, output))

   (* process - the shunting function, with the usual Rothwell interpretation
      (viz. apply any outgoing operators to things in the output list, rather
      than add them to the list in reverse polish notation). *)

    fun process(ienv, input: Object list, stack: StackEntry list,
		last: LastObj, output: Object list
	       ): Object =
      case input
	of nil =>			(* Nothing more to process *)
	     flushAll(stack, output)

	 | this :: rest =>
	     (case asId this
		of SOME id =>		(* Dealing with an unqual. id. *)
		     (case Fixity.lookup (ienv,id)
			of Fixity.Infix(n,false) =>
			     operator(ienv, INFIXentry(id, n),
				      rest, stack, output
				     )

			 | Fixity.Infix(n,true) =>
			     operator(ienv, INFIXRentry(id, n),
				      rest, stack, output
				     )

			 | Fixity.Nonfix =>
			     (case last
				of ARG =>	(* Must generate an appl. *)
				     operator(ienv, APPLentry, input,
					      stack, output
					     )

				 | _ =>		(* Just pass the nonfix. *)
				     process(ienv, rest, stack, ARG,
					     this :: output
					    )
			     )
		     )

		 | NONE =>		(* Not an unqual. identifier. *)
		     (case last
			of ARG =>	(* Must generate an application *)
			     operator(ienv, APPLentry, input, stack, output)

			 | _ =>		(* Just pass it through. *)
			     process(ienv, rest, stack, ARG,
				     this :: output
				    )
		     )
	     )

   (* operator - flush all stack entries with higher precedence, and then
      stack this one. *)

    and operator(ienv, entry, input, stack, output) =
      let
	val (stack', output') = flushHigher(entry, stack, output)
      in
	process(ienv, input, entry :: stack', OPER, output')
      end

   (* resolveInfix - takes a list of atomic exps/pats and returns a
      single exp/pat with the nonfix and infix applications in place.
      Usual Dijkstra shunting algorithm stuff, except that we have to
      identify runs of nonfixes (they must be applications), and we have to
      detect ill-formed cases (the input grammer was only specific enough to
      deliver a list of atexps, which includes things like "2 ::"). *)

    fun resolve (ienv, atoms) =
      process(ienv, atoms, nil, VOID, nil)
  end;
