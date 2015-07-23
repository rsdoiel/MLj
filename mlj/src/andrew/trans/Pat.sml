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
(* Pattern matching compiler.                 				*)
(*======================================================================*)
structure Pat :> PAT =
struct

local open TransOps
in

(*----------------------------------------------------------------------*)
(* A path identifies a point in a pattern expression by a sequence of	*)
(* field selections (#lab), constructor projections and pointer 	*)
(* dereferencing.							*)
(* NOTE: pattern compilation is not particularly sophisticated and may  *)
(* `retest' constructors. [See SPJ's book, page 89]. This is why        *)
(* projections indicate which constructor is `projected on'.            *)
(* A path environment assigns a path to each SML term variable.         *)
(*----------------------------------------------------------------------*)
datatype PathItem = 
  Select of Syntax.symbol
| ConProject of Syntax.symbol
| ExConProject of SMLTy.ExName
| Dereference                 

type Path = PathItem list
type PathEnv = Path Symbol.OrdMap.map

(*----------------------------------------------------------------------*)
(* A decision tree has two kinds of leaves:				*)
(*    Failure err	if no pattern matches				*)
(*    Success (e,loc)	if pattern number e matches			*)
(* and nodes of the form						*)
(*    Node { path, cases, default } where path identifies the position  *)
(* in the pattern of the constructor, cases is a set of branches each   *)
(* corresponding to a different constructor (or exception or constant)  *)
(* and default is a branch to take if none of these match.		*)
(*----------------------------------------------------------------------*)
datatype Tree =
  Failure of bool
| Success of int * Syntax.Location
| Node of { path : Path, cases : Cases, default : Tree }

(*----------------------------------------------------------------------*)
(* The three forms of Cases correspond to tests against datatypes,	*)
(* special constants and exceptions respectively. 			*)
(* 1.	ConCases(CE, tys, m) represents a test against datatype CE;     *)
(*      m is a map from constructor names to decision trees; tys are    *)
(*      the type parameters to CE.                                      *)
(* 2.   SConCases is a map from Java constants to decision trees.	*)
(* 3.   ExConCases is a list that pairs excons with decision   		*)
(*      trees and typed arities. It is a list (a) because two exnames   *)
(*      may appear to be distinct but at link-time are identified, and  *)
(*      (b) because we permit Java classes to be used for exceptions.   *)
(*----------------------------------------------------------------------*)
and Cases =
  ConCases  of (bool * SMLTy.DatDef) * SMLTy.Type list * Tree Symbol.OrdMap.map
| SConCases of Tree CMap.map
| ExConCases of (SMLTy.ExName * Tree * SMLTy.Type option) list

datatype Matcher =
  Con of Syntax.symbol
| SCon of Constants.constant
| ExCon of SMLTy.ExName

datatype Info = 
  Positive of Matcher
| Negative of Matcher list

(*----------------------------------------------------------------------*)
(* Pretty print a path for diagnostic purposes.          		*)
(*----------------------------------------------------------------------*)
fun pathToString path =
  Pretty.vec ("[]", "[", "]", "[", "]", "")
  (fn item =>
   case item of
    Select lab => "#" ^ Pretty.idToString lab
  | Dereference => "!"
  | ConProject c => "^" ^ Pretty.idToString c
  | ExConProject c => "%") path

(*----------------------------------------------------------------------*)
(* Pretty print a path environment for diagnostic purposes.    		*)
(*----------------------------------------------------------------------*)
fun PEtoString PE =
  Pretty.vec ("", "{", "}", "{", "}", ",") 
  (fn (smlvar,path) => Pretty.idToString smlvar ^ ":" ^ pathToString path) 
  (Symbol.OrdMap.listItemsi PE)

(*----------------------------------------------------------------------*)
(* Pretty print a decision tree.                                        *)
(*----------------------------------------------------------------------*)
fun treeToString (Failure true) = "Match"
  | treeToString (Failure false) = "fail"
  | treeToString (Success(n,_)) = "success " ^ Int.toString n
  | treeToString (Node { path, cases, default }) =
    "(case " ^ pathToString path ^ " of\n" ^ casesToString cases ^ 
    "\n| _ => " ^ treeToString default ^ ")\n"

and casesToString (ConCases(_, _, m)) = 
  Pretty.vec("", "", "", "", "", "\n | ")
  ( fn (c, tree) => Pretty.idToString c ^ " => " ^ treeToString tree)
  (Symbol.OrdMap.listItemsi m)
  | casesToString _ = "?"

(*----------------------------------------------------------------------*)
(* Lexicographic comparison of two paths.				*)
(* Selections and projections should never be compared.			*)
(*----------------------------------------------------------------------*)
fun compare (pathpair : Path * Path) =
  case pathpair of
    ([], []) => EQUAL
  | ([], _::_) => LESS
  | (_::_, []) => GREATER
  | (Select x::xs, Select y::ys) => 
    (case Symbol.OrdKey.compare (x,y) of
      EQUAL => compare (xs,ys)
    | other => other)

  | (ConProject x::xs, ConProject y::ys) =>
    (case Symbol.OrdKey.compare (x,y) of
      EQUAL => compare (xs,ys)
    | other => other)

  | (ExConProject x::xs, ExConProject y::ys) =>
    (case SMLTy.ExMap.Key.compare (x,y) of
      EQUAL => compare (xs, ys)
    | result => result)

  | (Dereference::xs, Dereference::ys) =>
    compare (xs, ys)

  | _ => Debug.fail "Pat.compare: invalid paths"

fun lookupInfo ([], path) = Negative []
  | lookupInfo ((path',info)::rest, path) =
    if compare(path,path') = EQUAL
    then info else lookupInfo (rest,path)

(*----------------------------------------------------------------------*)
(* Given positive and negative information about previous tests, 	*)
(* optimize a node.                                                     *)
(*----------------------------------------------------------------------*)
fun reduce info t =
case t of
  Node { path, cases, default } =>
  let
    val (cases,default) = 
    case cases of
      ConCases(CE, tys, m) =>
      (ConCases(CE, tys, Symbol.OrdMap.mapi 
        (fn (c,t) => reduce ((path,Positive (Con c))::info) t) m),
       reduce 
          ((path,Negative (map (Con o #1) 
            (Symbol.OrdMap.listItemsi m)))::info) default)

    | SConCases m =>
      (SConCases (CMap.mapi 
        (fn (c,t) => reduce ((path,Positive (SCon c))::info) t) m),
       reduce info default)

    | ExConCases l =>
      (ExConCases (map 
        (fn (c,t,a) => (c,reduce ((path,Positive (ExCon c))::info) t,a)) l),
       reduce info default)
  in
    case (lookupInfo (info, path), cases) of
      (Positive (Con c), ConCases (CE, tys, m)) =>
      (case Symbol.OrdMap.find(m, c) of
        NONE => default
      | SOME t => t)

    | (Negative matchers, ConCases (CE, tys, m)) =>
      let 
        val m = foldr (fn (Con c, m) =>
          (#1 (Symbol.OrdMap.remove (m,c))) handle _ => m) m matchers
        val default = 
          if Symbol.OrdMap.numItems m + length matchers = Symbol.OrdMap.numItems 
            (#3 (#2 CE))
          then Failure false else default
      in
        if Symbol.OrdMap.numItems m = 0
        then default 
        else 
          Node { path = path, cases = ConCases (CE, tys, m), default = default}
      end

    | _ => 
      Node { path = path, cases = cases, default = default }
  end

| _ => t
  
(*----------------------------------------------------------------------*)
(* Apply a function on decision trees down a set of cases		*)
(*----------------------------------------------------------------------*)
fun mapCases (f : Tree -> Tree) (cases : Cases) =
case cases of
  ConCases(CE, tys, m) => 
  ConCases(CE, tys, Symbol.OrdMap.map f m)

| SConCases m =>
  SConCases(CMap.map f m)

| ExConCases l =>
  ExConCases(map (fn (excon, t, arity) => (excon, f t, arity)) l)

(*----------------------------------------------------------------------*)
(* We merge exception constructor cases where possible, but do *not*	*)
(* re-order the cases, as there may be cases which are apparently       *)
(* distinct yet actually the same due to an excon rebinding.            *)
(* (And the order matters when Java exception classes are involved)     *)
(*----------------------------------------------------------------------*)
fun mergeExConCases ([], cases2) = cases2
  | mergeExConCases ((c1 as (exname1,tree1,tyopt1))::cases1, cases2) =
  let
    fun findAndRemove prefix [] = (prefix, NONE)
      | findAndRemove prefix ((c2 as (exname2,tree2,tyopt2))::suffix)= 
        if SMLTy.ExMap.Key.compare(exname1, exname2) = EQUAL
        then (prefix @ suffix, SOME c2)
        else findAndRemove (prefix @ [c2]) suffix 
  in
    case findAndRemove [] cases2 of
      (cases2', NONE) => c1 :: mergeExConCases (cases1, cases2')
    | (cases2', SOME (c2 as (_,tree2,_))) =>
      (exname1, mergeTrees (tree1, tree2), tyopt1) 
      :: mergeExConCases (cases1, cases2')
  end

(*----------------------------------------------------------------------*)
(* Merge two trees, the first taking priority over the second.		*)
(*----------------------------------------------------------------------*)
and mergeTrees (t1 : Tree, t2 : Tree) =
case (t1, t2) of

(* Merge two nodes, pointwise if the nodes describe the same path, otherwise
   choosing a `direction' based on a lexicographic comparison of the paths *)
  (Node(n1 as { path = p1, cases = c1, default = d1}), 
   Node(n2 as { path = p2, cases = c2, default = d2})) =>
  (case compare (p1, p2) of

    (* If paths are equal, merge the cases pointwise *)
    EQUAL   => 
    (case (c1, c2) of
      (ConCases(CE1, tys1, m1), ConCases(CE2, tys2, m2)) => 
      Node 
      { 
        path = p1, 
        cases = ConCases (CE1, tys1, 
           Symbol.OrdMap.unionWith mergeTrees (
             Symbol.OrdMap.mapi (fn (id, t) =>
               if isSome(Symbol.OrdMap.find(m2, id)) then t 
               else mergeTrees(t,d2)) m1, 
             Symbol.OrdMap.mapi (fn (id, t) => 
               if isSome(Symbol.OrdMap.find(m1, id)) then t 
               else mergeTrees(d1,t)) m2)),
        default = mergeTrees (d1,d2) 
      }

    | (SConCases m1, SConCases m2) => 
      Node 
      { 
        path = p1, 
        cases = SConCases(
           CMap.unionWith mergeTrees 
           (
             CMap.mapi (fn (id, t) =>
              if isSome(CMap.find(m2, id)) then t else mergeTrees(t,d2)) m1, 
             CMap.mapi (fn (id, t) => 
              if isSome(CMap.find(m1, id)) then t else mergeTrees(d1,t)) m2
           )),
        default = mergeTrees(d1,d2) 
      }

    | (ExConCases l1, ExConCases l2) => 
      Node 
      { 
        path = p1, 
        cases = ExConCases(mergeExConCases(l1,l2)),
        default = mergeTrees (d1,d2) 
      }

    | _ => 
      Debug.fail "Pat.mergeTrees: incompatible cases"
    )

  | LESS    => 
    Node 
    { 
      path = p1, 
      cases = mapCases (fn t1 => mergeTrees(t1,t2)) c1, 
      default = mergeTrees (d1,t2) 
    }

  | GREATER => 
    Node 
    { 
      path = p2, 
      cases = mapCases (fn t2 => mergeTrees(t1,t2)) c2, 
      default = mergeTrees (t1,d2) 
    }
  )

(* Success over-rules anything else if it appears first *) 
| (Success _, _) => t1

(* If it appears second, propagate the merge down the node *)
| (Node { path = p, cases = c, default = d }, Success _) =>
   Node 
   { 
     path = p, 
     cases = mapCases (fn t1 => mergeTrees(t1,t2)) c, 
     default = mergeTrees (d,t2) 
   }

(* Elide failure *)
| (Failure _, _) => t2

| (_, Failure _) => t1

(*----------------------------------------------------------------------*)
(* Generate a decision tree and path environment from a pattern.	*)
(* At the same time generate error messages if any special constants	*)
(* are out of range.							*)
(*----------------------------------------------------------------------*)
fun patToTree (pat : SMLTerm.Pat, ty : SMLTy.Type, transType) (tree : Tree) =
let 
  open SMLTerm
  fun toTree path (pat,ty) (acc as (tree,VE)) =
  case pat of

    PatWild => 
    acc

  | PatSCon(scon, loc) => 
    (case TransSCon.trans (scon, transType ty) of
      NONE => 
      (addError (Error.error (loc, "constant in pattern too large")); acc)

    | SOME jcon => 
      (
        Node 
        { 
          path = path, 
          cases = SConCases (CMap.insert(CMap.empty, jcon, tree)),
          default = Failure true
        }, 
        VE
      )
    )

  | PatVar(v,ty) =>
    (tree, Symbol.OrdMap.insert (VE, v, path))

  | PatCon(con, defs as (isrec,(tyvars, tyname, CE)), tys, patopt) =>
    let 
      val (i, tyopt) = SMLTy.findCon(tyname, CE, con)
      val tyopt = Option.map (SMLTy.appSubst (ListPair.zip(tyvars,tys))) tyopt
      val (tree,VE) = argToTree (path @ [ConProject con]) (patopt, tyopt) acc
    in
      (
        Node 
        { 
          path = path, 
          cases = ConCases (defs, tys, 
            Symbol.OrdMap.insert (Symbol.OrdMap.empty,con,tree)),
          default = Failure true
        }, 
        VE
      )
    end

  | PatExCon(excon, NONE) =>
    (
      Node 
      { 
        path = path, 
        cases = ExConCases [(excon, tree, NONE)], 
        default = Failure true
      }, 
      VE
    )
      
  | PatExCon(excon, SOME (ty,pat)) =>  
    let
      val (tree,VE) = toTree (path @ [ExConProject excon]) (pat,ty) acc
    in
      ( 
        Node 
        { 
          path = path, 
          cases = ExConCases [(excon, tree, SOME ty)], 
          default = Failure true
        }, 
        VE
      )
    end

  | PatRef pat =>
    toTree (path @ [Dereference]) (pat, valOf (SMLTy.fromRefType ty)) acc

  | PatRecord(openrec, patrow) =>
    rowToTree path (patrow, ty) acc

  | PatLayer((v,_), pat) => 
    let
      val (tree,VE) = toTree path (pat, ty) acc
    in
      (tree, Symbol.OrdMap.insert (VE, v, path))
    end

and argToTree path (patopt, tyopt) acc =
  case (patopt, tyopt) of
    (NONE, NONE) => 
    acc

  | (SOME pat, SOME ty) => 
    toTree path (pat,ty) acc

  | _ => 
    Debug.fail "Pat.argToTree: arity mismatch between pattern and type"

and rowToTree path (patrow, ty) acc =
case patrow of
  [] => 
  acc

| (lab, pat)::patrow =>
  let 
    val (fldty, i, n) = SMLTy.fieldType (ty, lab)
    val acc = toTree (path @ [Select lab]) (pat, fldty) acc
  in
    rowToTree path (patrow, ty) acc
  end

in 
  toTree [] (pat,ty) (tree,Symbol.OrdMap.empty)
end

(*----------------------------------------------------------------------*)
(* Generate a decision tree and list of path environments from a	*)
(* match construct.							*)
(*----------------------------------------------------------------------*)
fun matchToTree (mrules, ty : SMLTy.Type, transType) =
let
  fun matchToTree' n [] = (Failure true, [])
    | matchToTree' n ((loc,pat,exp)::match) = 
      let
        val (tree1, VE) = patToTree (pat,ty,transType) (Success (n,loc))
        val (tree2, VEs) = matchToTree' (n+1) match
        val tree = mergeTrees (tree1, tree2)
      in
        (tree, VE::VEs)
      end
  val (tree,VEs) = matchToTree' 0 mrules
  val tree' = reduce [] tree
  val _ = if Controls.isOn "showPats"
        then (Debug.print ("\ntree = " ^ treeToString tree);
              Debug.print ("\ntree' = " ^ treeToString tree'))
        else ()
  
in
  (tree',VEs)
end      

(*----------------------------------------------------------------------*)
(* A binding environment maps paths to variable names and types.	*)
(*----------------------------------------------------------------------*)
type BindEnv = (Path * Var.Var * SMLTy.Type) list

(*----------------------------------------------------------------------*)
(* Pretty print a binding environment.					*)
(*----------------------------------------------------------------------*)
fun BEtoString BE =
Pretty.vec ("{}", "{", "}", "{", "}", ",")
(fn (path, v, ty) => pathToString path ^ " = " ^ Var.toString v) BE

(*----------------------------------------------------------------------*)
(* Split a path into two parts:  					*)
(*   a prefix up to and including the final Project                     *)
(*   and a suffix which consists solely of Selects and Dereferences.    *)
(*----------------------------------------------------------------------*)
fun splitPath path =
let
  fun split [] suffix = ([], suffix)
    | split (Select lab :: path') suffix = split path' (Select lab::suffix)
    | split (Dereference :: path') suffix = split path' (Dereference::suffix)
    | split (path as (ConProject _ :: path')) suffix = (rev path, suffix)
    | split (path as (ExConProject _ :: path')) suffix = (rev path, suffix)
in
  split (rev path) []
end

(*----------------------------------------------------------------------*)
(* Look up a path in a binding environment				*)
(*----------------------------------------------------------------------*)
fun lookupPath ([], path) = 
    Debug.fail ("Pat.lookupPath: path " ^ pathToString path ^ " missing")
  | lookupPath ((path',v,ty)::rest, path) =
    if compare(path,path')=EQUAL then (v,ty) else lookupPath (rest, path)

(*----------------------------------------------------------------------*)
(* Compile an exception match clause.					*)
(* Inputs:                                                              *)
(*    defe              expression in case of no match                  *)
(*    exname            the exception name itself                       *)
(*    tree              the tree to be compiled on a match              *)
(*    arity             the arity (NONE or SOME smlty) of the exname    *) 
(*----------------------------------------------------------------------*)
fun compExConBind 
  (transExp,transType) VE EE (PEs : PathEnv list) bindings 
  (defe,path) (exname,tree,arity) =
let
  open MILTerm

  val SOME (varopt, milexty) = SMLTy.ExMap.find(EE, exname)

  (* If exception is generative then return extra bound variable for *)
  (* exception and appropriate code to test tag *)
  fun getextra () = 
    case varopt of
      NONE => 
      ([], fn e => e)

    | SOME v =>   
      let
        val v' = freshVar ()
      in
      (
        [v'], 
        fn e => Cond(JavaTest Tests.eq, Var v, Var v', e, defe)
      )
      end

  val (extra, wrap) = getextra ()
in
  case arity of
    NONE => 
    let
      val (ce, cty, exh) = 
        compile (transExp,transType) VE EE (PEs : PathEnv list) bindings tree
    in
      ((milexty, (extra, wrap ce)), cty, exh)
    end

  | SOME conty =>
    let
      val v = freshVar ()
      val (ce, cty, exh) = 
        compile (transExp,transType) VE EE (PEs : PathEnv list) 
        ((path @ [ExConProject exname], v, conty)::bindings) tree
    in
      ((milexty, (extra @ [v], wrap ce)), cty, exh)
    end
end

(*----------------------------------------------------------------------*)
(* Compile a decision tree to produce:                                  *)
(*    a MIL computation term;                                           *)
(*    a MIL computation type;                                           *)
(*    a flag indicating whether the tree is exhaustive or not.          *)
(*----------------------------------------------------------------------*)
and compile (transExp,transType) VE EE (PEs : PathEnv list) bindings tree =
let 
  open MILTerm

  (*--------------------------------------------------------------------*)
  (* Given a path and `root' variable name, produce code to follow the  *)
  (* path down from the root.                                           *)
  (* Return a MIL computation term, a MIL value type and an effect.     *)
  (*--------------------------------------------------------------------*)
  fun compilePath (BE, path) =
  let
  (*....................................................................*)
  (* Split the path into a prefix ending in a constructor projection and*)
  (* a suffix consisting entirely of field selections and dereferencing.*)
  (*....................................................................*)
    val (prefix, suffix) = splitPath path

  (*....................................................................*)
  (* Determine what MIL variable is bound to the constructor projection.*)
  (*....................................................................*)
    val (v,ty) = lookupPath (BE, prefix)

  (*....................................................................*)
  (* Compile the remainder using Proj and Deref.               		*)
  (*....................................................................*)
    fun compileSuffix path ty v =
    case path of
      [] => 
      (Triv [Var v], transType ty, Effect.none)

    | (Select lab :: path') =>
      let 
        val (fldty, i, n) = SMLTy.fieldType (ty, lab)
        val v' = freshVar ()
        val (e, milty, eff) = compileSuffix path' fldty v'
      in
        (
          LetVal(v', Proj(i, Var v), e), milty, eff
        )
      end

    | (Dereference :: path') =>
      let 
        val refty = valOf (SMLTy.fromRefType ty)
        val v' = freshVar ()
        val (e, milty, eff) = compileSuffix path' refty v'
      in
        (
          Let(Deref (Var v), ([(v', transType refty)], e)), milty,
          Effect.union(Effect.reads, eff)
        )
      end

    | ConProject _ :: path' =>
      Debug.fail "Pat.compilePath: found constructor projection in suffix"

    | ExConProject _ :: path' =>
      Debug.fail "Pat.compilePath: found constructor projection in suffix"

  in
    compileSuffix suffix ty v
  end

  (*....................................................................*)
  (* Actually compile the tree down to a bunch of case constructs.	*)
  (*....................................................................*)
  fun comp bindings tree =
    case tree of
      Failure _ => 
      let
        val (e, cty) = transExp VE NONE
      in
        (e, cty, false)
      end

    | Success (n, loc) => 
      let 
        fun make (PE, VE) = 
        case PE of
          [] => 
          let
            val (e, cty) = transExp VE (SOME n)
          in
            (e, cty, true)
          end

        | (smlvar,path)::PE' =>
          let
            val x = freshVar ()
            val (e1, ty, eff) = compilePath (bindings, path)
            val (e2, cty, exh) = 
              make (PE', Symbol.OrdMap.insert(VE, smlvar, (x,ty,[] (*,[] *))))
          in
            (Let(e1, ([(x,ty)], e2)), MILTy.cmpTypePlus(cty, eff), exh)
          end
      in
        make (Symbol.OrdMap.listItemsi (List.nth(PEs, n)), VE) 
      end

    | Node { path, cases, default } =>
      let 
        fun compConBind (defs as (tyvars,tyname,CE),tys) (con, tree) =
        case SMLTy.findCon(tyname, CE, con) of
          (i, NONE) => 
          let
            val (ce, cty, exh) = comp bindings tree
          in
            ((i, ([], ce)), cty, exh)
          end

        | (i, SOME conty) => 
          let
            val v = freshVar ()
            val (ce, cty, exh) = comp ((path @ [ConProject con], v, 
            SMLTy.appSubst (ListPair.zip(tyvars, tys)) conty)::bindings) tree
          in
            ((i, ([v], ce)), cty, exh)
          end

        fun compSConBind (jcon, tree) =
          let
            val (ce, cty, exh) = comp bindings tree
          in
            ((jcon, ([],ce)), cty, exh)
          end

        fun compCases f [] = 
            Debug.fail "Pat.comp: no cases"

          | compCases f [x] = 
            let
              val (y, cty,exh) = f x
            in
              ([y], cty, exh)
            end

          | compCases f (x::xs) =   
            let
              val (y, cty, exh) = f x
              val (ys, cty', exh') = compCases f xs
            in
              (y::ys, MILTy.unionCmpTypes(cty,cty'), exh andalso exh')
            end

      in 
        case cases of
          ConCases ((isrec, defs as (tyvars, tyname, CE)), tys, cases) => 
          let 
            fun maybeUnfold v = if isrec then Unfold v else v
            val exhaustive = Symbol.OrdMap.numItems cases = Symbol.OrdMap.numItems CE
            val unary = Symbol.OrdMap.numItems CE = 1
            val (e, milty, eff) = compilePath (bindings, path)
            val (cases', cty, exh') = 
              compCases (compConBind (defs,tys)) (Symbol.OrdMap.listItemsi cases)
            val (defopt, cty, exh) = 
              if exhaustive 
              then (NONE, cty, true)
              else 
              let 
                val (defe, cty', exh) = comp bindings default 
              in 
                (SOME defe, MILTy.unionCmpTypes(cty,cty'), 
                (case default of Failure false => true | _ => exh))
              end
            val v = freshVar ()
            val v' = freshVar ()
          in
            (Let(e, ([(v,milty)], 
              if unary
              then 
                if isSome (hd (Symbol.OrdMap.listItems CE))
                then 
                  LetVal(hd (#1 (#2 (hd cases'))), maybeUnfold (Var v), 
                    #2 (#2 (hd cases')))
                else #2 (#2 (hd cases'))
              else 
                LetVal (v', maybeUnfold(Var v), 
                  Case(Var v', true, cases', defopt)))),
              MILTy.cmpTypePlus(cty,eff),
              exh andalso exh')
          end

        | SConCases cases => 
          let
            val (e, milty, eff) = compilePath (bindings, path)
            val (defe, defcty, exh) = comp bindings default
            val v = freshVar ()
            val (cases', cty, exh') = 
              compCases compSConBind (CMap.listItemsi cases)
          in
            (Let(e, ([(v,milty)], CaseSCon(Var v, true, cases', SOME defe))),
            MILTy.cmpTypePlus(MILTy.unionCmpTypes(cty, defcty), eff),
            exh andalso exh')
          end

        | ExConCases cases =>      
          let
            val (e, milty, eff) = compilePath (bindings, path)
            val (defe, defcty, exh) = comp bindings default
            val v = freshVar ()
            val defv = freshVar ()
            val (cases', cty, exh') = compCases 
              (compExConBind (transExp,transType) VE EE PEs bindings
              (App(Var defv, []),path)) cases
          in
            (
            Let(e, ([(v,milty)], 
            LetFun([], AnyFun, Fun(defv, ([], defe)),
            CaseExCon(Var v, true, cases', SOME (App(Var defv, [])))))),
            MILTy.cmpTypePlus(MILTy.unionCmpTypes(cty, defcty), eff),
            exh andalso exh')
          end
      end

in
  comp bindings tree 
end

fun compileTop (transExp,transType) VE EE (PEs : PathEnv list) (v,tree,ty) =
  compile (transExp,transType) VE EE PEs [([], v, ty)] tree 

fun transFn 
{
  entity, transExp, transType, VE, EE, 
  smlty = ty,
  match = (ty',mrules as ((loc,_,_)::_))
} 
=
  let
    val (tree,PEs) = matchToTree (mrules,ty,transType)
    val v = freshVar ()
    val (ce, cty, exh) = 
    compileTop 
    (fn VE => 
     fn SOME n => transExp VE (#3 (List.nth(mrules, n)))
      | NONE => 
        let 
          val v = freshVar ()   
          val milty = transType ty'
        in 
          (MILTerm.LetVal(v, MILTerm.ExCon(MILTy.exn(TransExn.matchExn,[]),[]),
          MILTerm.Throw(MILTerm.Var v, [milty],(entity, loc))),
          MILTy.cmp(Effect.throws, [milty]))
        end, transType)
    VE
    EE
    PEs
    (v,tree,ty)
  in
    if exh 
    then ()
    else addError (Error.warning (loc, "match nonexhaustive"));
    (([(v,transType ty)], ce), cty)
  end

fun transLetPat 
{ 
  transExp, transType, VE, EE, var = v, smlty = ty, fail, pat, loc
}
=
  let
    val (tree, PEs) = matchToTree ([(loc, pat, ())], ty, transType)
    val (ce, cty, exh) = compileTop 
    (fn VE =>
     fn SOME _ => 
        let 
          val triples = Symbol.OrdMap.listItems VE
          val cty = MILTy.cmp(Effect.none, map #2 triples)
        in
          (MILTerm.Triv (map (fn (v,_,tys (* ,tynames *)) => 
            MILTermOps.tapp(MILTerm.Var v, tys)) triples), cty)
        end

      | NONE => fail, transType)
    Symbol.OrdMap.empty
    EE
    PEs
    (v, tree, ty)
  in
    if exh 
    then ()
    else addError (Error.warning (loc, "bind nonexhaustive"));
    (ce, cty)
  end

fun transHandle 
{ 
  entity, transExp, transType, 
  VE, EE, exp,
  match = (ty,mrules as (loc,_,_)::_)
} 
=
let
  val (ce,cty) = transExp VE exp
  val (tree,PEs) = matchToTree (mrules, SMLPrimTy.exnType, transType)
  val resvar = freshVar ()
  val milty = transType ty
  val milcty = MILTy.cmp(Effect.throws, [milty])
in
  case tree of
    Node { path = [], cases = ExConCases ecases, default } =>
    let

  (*..................................................................*)
  (* Construct a handler for a trylet construct.		      *)
  (*..................................................................*)
      fun makeHandler (exname, tree, arity) =
      let 
        (* The variable that is bound to the actual exception *)
        val exnvar = freshVar ()

        (* The same value, cast up to the top exception class *)
        val topexnvar = freshVar ()

        open MILTerm

        (* Look up the SML exname and get the MIL exception type *)
        val SOME (_, milexty) = SMLTy.ExMap.find(EE, exname)

        (* Default: re-raise the exception *)
        val defe = Throw(Var topexnvar, [milty], (entity,loc))

        fun transExp' VE nopt =
          case nopt of
            NONE => (defe, milcty)
          | SOME n => transExp VE (#3 (List.nth(mrules, n)))

        val ((_, (xs,e)), cty, exh)  =
          compExConBind (transExp', transType) VE EE PEs
            [([], topexnvar, SMLPrimTy.exnType)] (defe,[]) (exname,tree,arity)
      in
      (
        ([(exnvar, milexty)], 
          LetVal(topexnvar, Coerce(Var exnvar, MILTys.topExn), 
          Gen.foldri (fn (i,x,e) => LetVal(x, Proj(i, Var exnvar), e)) e xs)),
        cty
      )
      end

      val handlers = map makeHandler ecases
      val (defhandler, cty) = 
        case default of 
          Failure _ => ([], cty)
        | tree => 
          let 
            val topexnvar = freshVar ()
            open MILTerm
            val defe = Throw(Var topexnvar, [milty], (entity,loc))
            val (ce', cty', exh)  = compile
            (fn VE => 
              fn SOME n => transExp VE (#3 (List.nth(mrules,n)))
               | NONE => (defe, milcty),
                         transType) VE EE PEs 
                 [([], topexnvar, SMLPrimTy.exnType)] tree
          in
            ([([(topexnvar, MILTys.topExn)], ce')], 
            MILTy.unionCmpTypes (cty, cty'))
          end

    in
      (MILTerm.TryLet(ce, map #1 handlers @ defhandler,
      ([(resvar, milty)], MILTerm.Triv [MILTerm.Var resvar])),
      foldl MILTy.unionCmpTypes cty (map #2 handlers))
    end

  | _ =>
    let
      val exnv = freshVar ()
      val (ce', cty', exh) = 
      compileTop 
      (fn VE => 
       fn SOME n => transExp VE (#3 (List.nth(mrules,n)))
        | NONE => 
          (MILTerm.Throw(MILTerm.Var exnv, 
            [milty], (entity,loc)), milcty), transType) VE EE PEs
      (exnv, tree, SMLPrimTy.exnType)
    in
      (MILTerm.TryLet(ce, [([(exnv, MILTys.topExn)], ce')], 
      ([(resvar, milty)], MILTerm.Triv [MILTerm.Var resvar])),
      MILTy.unionCmpTypes(cty, cty'))
    end
end

end (* of local open *)
end (* of struct *)
