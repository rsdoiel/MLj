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
(* Generate basic-block code for a Java operation			*)
(*======================================================================*)
structure CompileJavaOp (* :> COMPILEJAVAOP *) =
struct

fun compile resultrequired 
  (javaop as (optype, optrep, optname), args, reps, repopt, eff) =
  let
    fun default operation = 
    let
      val resultvalue = 
        Option.map (fn _ => Blocks.new_value()) repopt
    in
      ([CompileOps.instr (operation, args, Gen.optToList resultvalue)],
       resultvalue)
    end
  in
      case optype of
        Java.Add => 
        default Blocks.add

      | Java.And =>     
        default Blocks.bitwise_and

      | Java.ArrayLength => 
        default (Blocks.arraylength Blocks.MUSTDO)

      | Java.ArrayLoad => 
        let val resval = Blocks.new_value ()
        in
          (CompileOps.instr (Blocks.aload (ArrayHandle.unknown, Blocks.MUSTDO),
          args, [resval])
          :: MicrosoftBug.kludgeInstrs (resval, valOf repopt), SOME resval)
        end

      | Java.ArrayStore => 
        default (Blocks.astore (ArrayHandle.unknown, Blocks.MUSTDO))

      | Java.CmpL =>     
        default (Blocks.cmp false)

      | Java.CmpG =>
        default (Blocks.cmp true)

      | Java.Cast => 
        let 
          val torep = valOf repopt
          val arg = hd args
          val fromrep = hd reps
          val fromjty = JavaRepOps.toJava fromrep
          val tojty = JavaRepOps.toJava torep
        in
          (* if Types.java_type_equal(fromjty, tojty) then ([], SOME arg)
          else *) if JavaRepOps.isPointer (valOf repopt)
          then default (Blocks.checkcast (tojty, Blocks.MUSTDO))
          else default (Blocks.convert (JavaRepOps.toBase torep))
        end

      | Java.Div =>     
        default (Blocks.divide Blocks.MUSTDO)

      | Java.ExnName =>
        let val resval = Blocks.new_value ()
            val castval = Blocks.new_value ()
        in
        (
          [CompileOps.instr (Blocks.checkcast 
            (Types.F(0, Types.CLASS (JavaNames.exnClass NONE)), Blocks.MUSTDO),
            args, [castval]),
           CompileOps.instr (Blocks.invoke_virtual
            (CompileOps.mref (JavaNames.exnClass NONE,
              JavaString.fromString JavaNames.exnNameMethod, [], 
              repopt)), [castval], [resval])
          ],
          SOME resval
        )
        end

      | Java.ExnMessage =>
        let val resval = Blocks.new_value ()
            val castval = Blocks.new_value ()
        in
        (
          [CompileOps.instr (Blocks.checkcast 
            (Types.F(0, Types.CLASS (JavaNames.exnClass NONE)), Blocks.MUSTDO),
            args, [castval]),
           CompileOps.instr (Blocks.invoke_virtual
            (CompileOps.mref (JavaNames.exnClass NONE, 
              JavaString.fromString JavaNames.exnMessageMethod, 
              [], repopt)), [castval], [resval])
          ],
          SOME resval
        )
        end

      | Java.IsMLExn =>
        default (Blocks.instanceof 
          (Types.F(0, Types.CLASS (JavaNames.exnClass NONE))))

      | Java.GetField =>
        default (case optrep of
          NONE =>
          Blocks.getfield
          (
            CompileOps.fref(JavaRepOps.toClass (hd reps), 
              valOf optname, valOf repopt, false), Blocks.MUSTDO
          )
        | SOME rep =>
          Blocks.getstatic
          (
            CompileOps.fref(JavaRepOps.toClass rep, valOf optname, valOf repopt,
              false),  Blocks.MUSTDO
          )
        )

      | Java.InstanceOf => 
        default (Blocks.instanceof (JavaRepOps.toJava (valOf optrep)))

      | Java.InvokeSpecial =>
        default (Blocks.invoke_special
        (
          CompileOps.mref (JavaRepOps.toClass (hd reps), 
            valOf optname, tl reps, repopt)
        ))

      | Java.Invoke =>
        default (case optrep of
          NONE => 
          Blocks.invoke_virtual
          (
            CompileOps.mref (JavaRepOps.toClass (hd reps), 
              valOf optname, tl reps, repopt)
          )

        | SOME rep =>
          Blocks.invoke_static
          (
            CompileOps.mref (JavaRepOps.toClass rep, valOf optname, reps, repopt)
          )
        )

      | Java.InvokeInterface =>
        default (Blocks.invoke_interface
        (
          CompileOps.mref (JavaRepOps.toClass (hd reps), 
            valOf optname, tl reps, repopt)
        ))

      | Java.MonitorEnter =>
        default Blocks.monitorenter

      | Java.MonitorExit =>
        default Blocks.monitorexit
 
      | Java.Mul =>     
        default Blocks.mul

      | Java.Neg =>     
        default Blocks.neg

      | Java.New => 
        default (Blocks.new
        (
          CompileOps.mref (JavaRepOps.toClass (valOf repopt), 
            JavaString.fromString "<init>", 
            reps, NONE)
        ))

      | Java.NewArray => 
        default 
        (Blocks.newarray (JavaRepOps.toJava (valOf repopt), Blocks.MUSTDO))
      
      | Java.Or =>      
        default Blocks.bitwise_or

      | Java.PutField =>
        default (case optrep of
          NONE =>
          Blocks.putfield
          (
            CompileOps.fref (JavaRepOps.toClass (hd reps), valOf optname, 
              List.nth(reps, 1), false), Blocks.MUSTDO
          )
        | SOME rep =>
          Blocks.putstatic
          (
            CompileOps.fref (JavaRepOps.toClass rep, valOf optname, hd reps,
              false), Blocks.MUSTDO
          )
        )

      | Java.Rem =>     
        default (Blocks.rem Blocks.MUSTDO)

      | Java.Shl =>     
        default Blocks.shl

      | Java.Shr =>     
        default Blocks.shr

      | Java.SignExtend =>
        default Blocks.sign_extend

      | Java.Sub =>     
        default Blocks.sub

      | Java.Ushr =>    
        default Blocks.ushr

      | Java.Xor =>     
        default Blocks.bitwise_xor

      | _ =>
        Debug.fail ("CompileJavaOp.compile: illegal instruction " ^ 
        JavaOps.opTypeToString optype)

  end

end
