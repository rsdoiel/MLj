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

structure CompileOps =
struct

(*----------------------------------------------------------------------*)
(* Operations to delete element(s) from a set without raising an        *)
(* exception when the element is not present.                           *)
(*----------------------------------------------------------------------*)
fun delete (s, v) = (Var.Set.delete(s,v)) handle _ => s
fun remove (s, vs) = foldl (fn (v,s) => delete(s, v)) s vs

(*----------------------------------------------------------------------*)
(* A type environment maps each variable to:                            *)
(*   a MIL type                                                         *)
(*   the value (local, global, constant) bound to it                    *)
(* A block environment maps local function variables to typed blocks.   *)
(* [The optional second block is for handlers]                          *)
(*----------------------------------------------------------------------*)
type TyEnv    = (Blocks.value * MILTy.Type) Var.Map.map
type KindEnv  = MILTy.Kind Var.Map.map
type GlobEnv  = (int * MILTy.Type) Var.Map.map
type BlockEnv = 
  (Blocks.block option ref * Blocks.block option ref * MILTy.Type) Var.Map.map
type KnownEnv = (int * MILTy.Type) Var.Map.map
type TypedHandle = (Blocks.value * JavaRep.Rep)
type Env      = 
{
  known  : KnownEnv,
  blocks : BlockEnv,
  tyenv  : TyEnv,
  globenv: GlobEnv,
  kindenv: KindEnv,
  apps   : int Var.Map.map
}

val empty = { known = Var.Map.empty, blocks = Var.Map.empty, 
              tyenv = Var.Map.empty, globenv = Var.Map.empty,
              kindenv = Var.Map.empty,
              apps = Var.Map.empty }

fun extendTypes (env : Env as { known, blocks, tyenv,kindenv, apps, globenv }) 
    (v, value, ty) =
  { known = known, blocks = blocks, globenv = globenv, kindenv = kindenv,
    tyenv = if Var.isDummy v then tyenv 
            else Var.Map.insert(tyenv, v, (value, ty)), apps = apps }

fun extendTyVars (env : Env as { known, blocks, tyenv,kindenv, apps,globenv }) 
    tyvars =
  { known = known, blocks = blocks, globenv = globenv, 
    kindenv = foldl (fn ((x,kind),kindenv) => Var.Map.insert(kindenv, x, kind))
      kindenv tyvars, tyenv = tyenv, apps = apps }

fun extendEnv (env : Env as { known, blocks, tyenv, apps, kindenv, globenv })
  (vars, values, cty) =
  let
    val (effect,defntys) = MILTy.fromCmp cty
    val triples = ListPair.zip(vars, ListPair.zip(values, defntys))
    val tyenv' = foldl (fn ((var, (value, ty)), tyenv) =>
        if not (Var.isDummy var) then Var.Map.insert(tyenv, var, (value, ty))
        else tyenv)
        tyenv triples
  in
    { known = known, blocks = blocks, tyenv = tyenv', apps = apps,
      globenv = globenv, kindenv = kindenv }
  end

fun extendBlocks (env : Env as { known, blocks, tyenv, apps, globenv,kindenv })
  (v, value, ty) =
  { known = known, tyenv = tyenv, kindenv = kindenv,
    blocks = Var.Map.insert(blocks, v, (value, ref NONE, ty)), apps = apps, 
    globenv = globenv }

fun extendKnown (env : Env as { known, blocks, tyenv, apps, globenv, kindenv })
  (v, value, ty) =
  { blocks = blocks, tyenv = tyenv, kindenv = kindenv,
    known = Var.Map.insert(known, v, (value, ty)), apps = apps, 
    globenv = globenv}

fun whichAppMethod (env : Env as { apps, kindenv, ... }, x) =
Var.Map.find(apps, x)
     

local 
  structure B = Blocks
  open JavaRepOps
in

(*----------------------------------------------------------------------*)
(* Statistics								*)
(*----------------------------------------------------------------------*)
val maxInstrs = 65536

structure Stats =
struct

val grain = ref 5
val blocks = ref 0
val tryblocks = ref 0
val catchblocks = ref 0
val dist = ref (Array.array (0,0))
val instrs = ref 0

end


(*----------------------------------------------------------------------*)
(* Extract the integer contents of an integer-compatible Java constant  *)
(*----------------------------------------------------------------------*)
fun jconToInt (Constants.BOOLEAN i) = i
  | jconToInt (Constants.BYTE i) = i
  | jconToInt (Constants.SHORT i) = i
  | jconToInt (Constants.INT i) = i
  | jconToInt (Constants.CHAR i) = i
  | jconToInt _ = 
    Debug.fail "CompileOps.jconToInt: constant not integer-compatible"

fun addDist instrs =
(
  Stats.instrs := !Stats.instrs + instrs;
  if instrs < maxInstrs 
  then
  let
    val i = instrs div (!Stats.grain)
  in
    Array.update(!Stats.dist, i, Array.sub(!Stats.dist, i) + 1)
  end
  else ()
)

val lastblock = ref (NONE : Blocks.block option)

fun newBlock (args : TypedHandle list, instrs, exit) = 
  (
    Stats.blocks := !Stats.blocks + 1;
(*
    addDist (length instrs);
*)
    let
      val b = B.make_block 
        (B.D { input = map (JavaRepOps.toJava o #2) args },
          map #1 args, instrs, exit, [])
      in
        lastblock := SOME b; b
      end
  )

fun newTryBlock (args : TypedHandle list, instrs, exit, handlers) = 
  (
    Stats.tryblocks := !Stats.tryblocks + 1;
(*
    addDist (length instrs);
*)
    B.make_block 
    (B.D { input = map (JavaRepOps.toJava o #2) args },
      map #1 args, instrs, exit, 
      map (fn (rep, label) => B.E (SOME (toClass rep), label)) handlers)
  )

fun newCatchBlock 
  (exnarg : TypedHandle, args : TypedHandle list, instrs, exit) = 
  (
    Stats.catchblocks := !Stats.catchblocks + 1;
(*
    addDist (length instrs);
*)
    B.make_handler_block 
    (B.D { input = map (toJava o #2) args }, map #1 args, instrs, exit, [], 
     { thrown = #1 exnarg, 
       thrown_class = SOME (toClass (#2 exnarg)) } )
  )

(*----------------------------------------------------------------------*)
(* Create a new basic block value handle				*)
(*----------------------------------------------------------------------*)
fun newHandle () = B.new_value ()

(*----------------------------------------------------------------------*)
(* Create a constant value					        *)
(*----------------------------------------------------------------------*)
fun constant jcon = B.new_constant_value jcon

(*----------------------------------------------------------------------*)
(* Create a constant integer            				*)
(*----------------------------------------------------------------------*)
fun constInt i = constant (Constants.INT (JavaInt.fromInt i))

(*----------------------------------------------------------------------*)
(* Constant null value                                                  *)
(*----------------------------------------------------------------------*)
val constNull = B.new_constant_value Constants.NULL

fun isConst h = B.is_constant h

(*----------------------------------------------------------------------*)
(* Generate a field ref from Java representations         	        *)
(*----------------------------------------------------------------------*)
fun fref (class, name, fldrep, immutable) =
(*
  FieldHandle.unknown
  {
    class = class,
    name = JavaString.fromString name,
    field_type = toJava fldrep
  }
*)
  FieldHandlePriv.H
  {
    class = class,
    name = name,
    field_type = toJava fldrep,
    is_mutable = not immutable
  }

(*----------------------------------------------------------------------*)
(* Generate a method descriptor from Java representations		*)
(*----------------------------------------------------------------------*)
fun mref (class, name, argreps, resrepopt) =
  MethodHandle.unknown
  {
    class = class,
    name = name,
    input_types = map toJava argreps,
    output_type = Option.map toJava resrepopt
  }


(*----------------------------------------------------------------------*)
(* Generate instruction for accessing a field			1/9/97	*)
(*----------------------------------------------------------------------*)
fun getfield ((recval, recrep), label, immutable, (fldval, fldrep)) =
(
  B.getfield
  (
    fref (toClass recrep, label, fldrep, immutable),
    B.OPTIONAL
  ), 
  { input = [recval], 
    output = [fldval]
  }
)
 
(*----------------------------------------------------------------------*)
(* Generate instruction for setting a field			1/9/97	*)
(*----------------------------------------------------------------------*)
fun putfield ((recval, recrep), label, immutable, (fldval, fldrep)) =
(
  B.putfield
  (
    fref (toClass recrep, label, fldrep, immutable),
    B.OPTIONAL
  ), 
  {
    input = [recval, fldval],
    output = []
  }
)

(*----------------------------------------------------------------------*)
(* Generate an instruction for accessing a static field		4/9/97	*)
(*----------------------------------------------------------------------*)
fun getstatic (class, label, immutable, (fldval, fldrep)) =
(
  B.getstatic
  (
    fref (class, label, fldrep, immutable),
    B.OPTIONAL
  ),
  {
    input = [],
    output = [fldval]
  }
)
 
(*----------------------------------------------------------------------*)
(* Generate an instruction for setting a static field		4/9/97	*)
(*----------------------------------------------------------------------*)
fun putstatic (class, label, immutable, (fldval, fldrep)) =
(
  B.putstatic 
  (
    fref (class, label, fldrep, immutable),
    B.OPTIONAL
  ), 
  {
    input = [fldval],
    output = []
  }
)


fun hd' [] = NONE
  | hd' (x::xs) = SOME x

(*----------------------------------------------------------------------*)
(* Generate an instruction for invoking a static method         1/9/97	*)
(*----------------------------------------------------------------------*)
fun invokestatic (class, method, args : TypedHandle list, 
  resvals : TypedHandle list) =
(
  B.invoke_static
  (
    mref (class, method, 
          map #2 args, Option.map #2 (Gen.hd resvals))
  ),
  {  
    input = map #1 args,
    output = map #1 resvals
  }
)
    
(*----------------------------------------------------------------------*)
(* Generate an instruction for invoking a virtual method        1/9/97	*)
(*----------------------------------------------------------------------*)
fun invokevirtual (method, values : TypedHandle list, 
  resvals : TypedHandle list) =
(
  B.invoke_virtual
  (
    mref (toClass (#2 (hd values)), method, map #2 (tl values), 
          Option.map #2 (Gen.hd resvals))
  ),
  {
    input = map #1 values,
    output = map #1 resvals
  }
)

(*----------------------------------------------------------------------*)
(* Generate instruction for projecting from a tuple or constructor.     *)
(* Usually we just use getfield but as a workaround for the SDK bug we  *)
(* use selector methods instead.                                        *)
(*----------------------------------------------------------------------*)
fun proj ((recval, recrep), label, immutable, (fldval, fldrep)) =
if Controls.isOn "MicrosoftBug" andalso 
(case recrep of JavaRep.Prod _ => true | JavaRep.Con _ => true | _ => false)
then
(
  B.invoke_virtual
  (
    mref (toClass recrep, label, [], SOME fldrep)
  ),
  {
    input = [recval],
    output = [fldval]
  }
)
else
(
  B.getfield
  (
    fref (toClass recrep, label, fldrep, immutable),
    B.OPTIONAL
  ), 
  { input = [recval], 
    output = [fldval]
  }
)
 
    
(*----------------------------------------------------------------------*)
(* Generate code for creating a new object			1/9/97	*)
(*----------------------------------------------------------------------*)
fun init (args : TypedHandle list, resval : TypedHandle) =
(
  B.new (mref (toClass (#2 resval), 
    JavaString.fromString "<init>", map #2 args, NONE)),
  { 
    input = map #1 args, 
    output = [#1 resval]
  }
)


(*----------------------------------------------------------------------*)
(* Generate an instruction for a checkcast operation         	29/5/97 *)
(*----------------------------------------------------------------------*)
fun checkcast (inputval, outputval as (h,rep)) =
(
  B.checkcast (toJava rep, B.OPTIONAL),
  {
    input = [inputval],
    output = [h]
  }
)

(*----------------------------------------------------------------------*)
(* Generic instruction       						*)
(*----------------------------------------------------------------------*)
fun instr (operation, input, output) = 
  (operation, { input = input, output = output })


(*----------------------------------------------------------------------*)
(* Generate various exits			                3/9/97  *)
(*----------------------------------------------------------------------*)
fun throw objval = B.athrow [objval]
fun return values = B.return values
fun goto label = B.goto label

(*----------------------------------------------------------------------*)
(* Generate a switch exit                          			*)
(*----------------------------------------------------------------------*)
fun switch (switchval, cases, default) =
  B.lookupswitch 
  { 
    input = [switchval],
    lookuptable = cases,
    default = default
  }

(*----------------------------------------------------------------------*)
(* Generate an exit for a conditional               			*)
(*----------------------------------------------------------------------*)
fun cond
(
  test      : Tests.test,                    (* Test to apply *)
  arg1, arg2,                                (* Values to test *)
  yes,                                       (* Label for then branch *)
  no                                         (* Label for else branch *)
) =
  B.cond
  {   
    input = [arg1, arg2],
    yes = yes,
    no = no,
    test = test
  }


end (* of local open *)
end (* of struct *)

