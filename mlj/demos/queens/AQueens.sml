(* AQueens.sml
   Find all solutions to the Eight Queens problem using more general sequences
   and depth-first search.
*)
structure AQueens =
struct

structure Seq = ImpSeq

fun upto (m,n) = if m>n then [] else m :: upto (m+1,n)

infix mem
fun x mem ys = List.exists (fn y => y=x) ys

fun secr f y x = f(x,y)

fun depthFirst next x =
   let fun dfs [] = Seq.empty
        |  dfs (y::ys) = Seq.cons(y, fn()=> dfs (next y @ ys))
   in dfs [x]
   end

fun safeQueen oldqs newq =
    let fun nodiag (i, [])=true
          | nodiag (i, q::qs) = 
               Int.abs (newq-q)<>i andalso nodiag(i+1, qs)
    in not (newq mem oldqs) andalso nodiag (1,oldqs)
    end

fun nextQueen n qs =
   map (secr op:: qs) (List.filter (safeQueen qs) (upto(1,n)))

fun isFull n qs = (length qs = n)

fun depthQueen n = Seq.filter (isFull n) (depthFirst (nextQueen n) [])

(* now the silly bits to calculate an interesting transition *)

fun threat (x,y) (x',y') = (x = x') orelse (y = y') 
                          orelse (x+y = x'+y') orelse (x-y = x'-y')

fun nextstates ([],[],soln) = []
  | nextstates (posn::rest, right, soln) =
     let fun threatsplits [] = []
           | threatsplits (p :: ps) = 
              let val ts = map (fn (a,aas) => (a, p::aas)) (threatsplits ps)
              in if threat posn p then (p,ps)::ts
                                  else ts
              end
     in map (fn (p,ps)=> (rest, ps, (posn, p)::soln)) (threatsplits right)
     end

fun initialstate queens1 queens2 = 
  let val onetoeight = upto(1,8)
  in (ListPair.zip (onetoeight,queens1), ListPair.zip (onetoeight,queens2),
      [] : ((int*int)*(int*int)) list)
  end

fun isTerminal (left,right,soln) = null left

fun depthMorph queens1 queens2 =
 Seq.map (fn (a,b,c)=>c) (Seq.filter isTerminal (depthFirst nextstates (initialstate queens1 queens2)))

(* depthMorph takes a pair of int lists representing the two solutions and returns
   an (int*int)*(int*int) list Sequence
   which enumerates the possible ways of going from one to the next
*)

fun isdiag ((x:int,y:int),(x',y')) = if (x <> x') andalso (y <> y') then 1 else 0

(* number of diagonal moves in a list of pairs of pairs representing a transition *)
val diagcount = foldl (fn (move,n)=>n+(isdiag move)) 0

(* given a list of possible morphs, find the one with the greatest number of diagonals *)
val bestmorph = foldl (fn (morph, (bestsofar, bestcount)) => 
                      let val v = diagcount morph 
                      in if v > bestcount then (morph, v)
                                          else (bestsofar,bestcount)
                      end) ([],~1)

fun bestmorph'  (a :: (b :: cs)) = (b,1)

(* makeloopy takes a sequence and turns it into a cyclic one. Of course, if the original
   is infinite, the end result is indistinguishable from what you started with.
   I wish I were doing this in Haskell....
 *)
fun makeloopy small = 
  if Seq.null small then Seq.empty
  else Seq.cycle (fn f => Seq.cons(Seq.hd small,fn ()=>Seq.@(Seq.tl small, f())));

val infinitequeens = makeloopy (depthQueen 8)

fun infinitemorphs st = let val h1 = Seq.hd st
                            val t1 = Seq.tl st
                            val h2 = Seq.hd t1
                        in Seq.cons(#1 (bestmorph (Seq.toList (depthMorph h1 h2))), 
                                    fn ()=>infinitemorphs t1)
                        end

val theend = infinitemorphs infinitequeens

end