structure MLJUtils =
struct

val op= = Prim.=
val op <> = Prim.<>

structure Int =
struct

fun x < y  = Prim.lt(x:int,y)
fun x > y  = Prim.gt(x:int,y)
fun x <= y = Prim.le(x:int,y)
fun x >= y = Prim.ge(x:int,y)

fun x + y = Prim.add(x:int,y)
fun x - y = Prim.sub(x:int,y)
fun ~x    = Prim.neg(x:int)
fun x * y = Prim.mul(x:int,y)

fun quot (x,y) = Prim.div(x:int,y)
fun rem (x,y) = Prim.rem(x:int,y)

end

structure FixedInt =
struct

fun x < y  = Prim.lt(Prim.cmp(x, x), 0)
fun x > y  = Prim.gt(Prim.cmp(x, y), 0)
fun x <= y = Prim.le(Prim.cmp(x, y), 0)
fun x >= y = Prim.ge(Prim.cmp(x, y), 0)

fun x + y = Prim.add(x:Prim.long,y)
fun x - y = Prim.sub(x:Prim.long,y)
fun ~x    = Prim.neg(x:Prim.long)
fun x * y = Prim.mul(x:Prim.long,y)

fun quot (x,y) = Prim.div(x:Prim.long,y)
fun rem (x,y) = Prim.rem(x:Prim.long,y)

end

structure Word =
struct

fun x + y = Prim.toWord (Prim.add(Prim.fromWord x, Prim.fromWord y))
fun x - y = Prim.toWord (Prim.sub(Prim.fromWord x, Prim.fromWord y))
fun x * y = Prim.toWord (Prim.mul(Prim.fromWord x, Prim.fromWord y))
fun orb (x, y) = Prim.toWord(Prim.or(Prim.fromWord x, Prim.fromWord y))
fun xorb (x, y) = Prim.toWord(Prim.xor(Prim.fromWord x, Prim.fromWord y))
fun andb (x, y) = Prim.toWord(Prim.And(Prim.fromWord x, Prim.fromWord y))
fun notb x = Prim.toWord(Prim.xor(Prim.fromWord x, Java.fromInt ~1))

end

structure LargeWord =
struct

fun x + y = Prim.toWord64 (Prim.add(Prim.fromWord64 x, Prim.fromWord64 y))
fun x - y = Prim.toWord64 (Prim.sub(Prim.fromWord64 x, Prim.fromWord64 y))
fun x * y = Prim.toWord64 (Prim.mul(Prim.fromWord64 x, Prim.fromWord64 y))
fun orb (x, y) = Prim.toWord64(Prim.or(Prim.fromWord64 x, Prim.fromWord64 y))
fun xorb (x, y) = Prim.toWord64(Prim.xor(Prim.fromWord64 x, Prim.fromWord64 y))
fun andb (x, y) = Prim.toWord64(Prim.And(Prim.fromWord64 x, Prim.fromWord64 y))
fun notb x = Prim.toWord64(Prim.xor(Prim.fromWord64 x, Java.fromInt64 ~1))

end

structure Char =
struct

fun x < y  = Prim.lt(x:char,y)
fun x > y  = Prim.gt(x:char,y)
fun x <= y = Prim.le(x:char,y)
fun x >= y = Prim.ge(x:char,y)

fun fromDigit (c:char, base:int) = 
  _pure (java.lang.Character.digit(c, base))

fun isCntrl c = c < #" " orelse c = #"\127"
fun isPrint c = c >= #" " andalso c <> #"\127"
fun isAscii c  = c <= #"\127"
fun isHexDigit c = Int.>=(fromDigit (c, 16), 0)
fun isDigit c = Int.>=(fromDigit (c, 10), 0)
fun isSpace c = c = #"\v" orelse
  _pure (java.lang.Character.isSpace(c))
fun isLower c = 
  _pure (java.lang.Character.isLowerCase(c))
fun isUpper c = 
  _pure (java.lang.Character.isUpperCase(c))
fun isAlpha c = 
  _pure (java.lang.Character.isLetter(c))
fun isAlphaNum c = isAlpha c orelse isDigit c

fun toLower c = 
  _pure (java.lang.Character.toLowerCase(c))
fun toUpper c  = 
  _pure (java.lang.Character.toUpperCase(c))

end

structure String =
struct

local open Char Option  in

(* Returns the number of characters in string s *)
fun size (s : string) = _pure (s.#length ())

(* Returns the i'th character of s, counting from zero. This raises
   Subscript if i<0 or size s <= i *)
fun sub(s:string, i) = s.#charAt(i)

fun (s1 : string) ^ (s2 : string) = 
  Prim.unsafeValOf(_pure (s1.#concat(s2)))

fun str (c : char) = 
  Prim.unsafeValOf(_pure (java.lang.String.valueOf(c)))

fun scanString scan s = 
  let val len = size s
      fun getc i = 
        if Prim.ge(i:int, len) then Option.NONE
	else Option.SOME (sub(s, i), Prim.add(i, 1))
  in 
    case scan getc 0 of
      Option.NONE => Option.NONE
    | Option.SOME (res, _) => Option.SOME res
  end


(* sigh - special treatment for completely pointless hexadecimal
   "0x" or "0X".  trim0x removes "0x[digit]" from the stream cs,
   if present *)
fun trim0x getc cs0 =
    case getc cs0 of
         SOME (#"0",cs1) =>
         let
            fun check_dig cs2=
            (case getc cs2 of
                NONE => cs0
            |   SOME(ch,_) =>
                   if isHexDigit ch then cs2 else cs0
            )
         in
            (case getc cs1 of
               SOME (#"X",cs2) => check_dig cs2
            |  SOME (#"x",cs2) => check_dig cs2
            |  _ => cs0
            )
         end
      |  _ => cs0

(* Trim white space, 0w prefix, and 0x/0X/0wx/0wX prefix when base=16 *)
fun trim0wx base getc cs0 =
    case getc cs0 of
         SOME (#"0",cs1) =>
         let
            fun check_dig cs2=
            (case getc cs2 of
                NONE => cs0
            |   SOME(ch,_) =>
                if Int.>=(fromDigit(ch,base),0) then cs2 else cs0
            )

            fun check_xdig cs2 =
            case getc cs2 of
              SOME(#"X",cs3) => check_dig cs3
            | SOME(#"x",cs3) => check_dig cs3
            | _ => cs0
         in
           if base=16
           then
             case getc cs1 of
               SOME (#"w",cs2) => 
               check_xdig cs2

             | SOME (#"x",cs2) =>
               check_dig cs2

             | SOME (#"X",cs2) =>
               check_dig cs2
 
             | _ => cs0
           else
             case getc cs1 of
               SOME (#"w",cs2) => check_dig cs2
             | _ => cs0
         end
      | SOME (c,cs1) =>
        if isSpace c
        then trim0wx base getc cs1
        else cs0

      | _ => cs0

(* Trim white space and sign; return false for pos, true for neg *)
fun trimSign getc src =
    case getc src of
      NONE => (Datatypes.false, src)
    | SOME (#"+", src') => (Datatypes.false, src')
    | SOME (#"-", src') => (Datatypes.true, src')
    | SOME (#"~", src') => (Datatypes.true, src')
    | SOME (c, src') =>
      if isSpace c
      then trimSign getc src'
      else (Datatypes.false, src)

fun charscan ML getc src = 
let
  val base = if ML then 10 else 8
in
  case getc src of
    SOME (#"\\", src) =>
    (case getc src of
      NONE => NONE
    | SOME (#"a", src) => SOME (#"\a", src)
    | SOME (#"b", src) => SOME (#"\b", src)
    | SOME (#"t", src) => SOME (#"\t", src)
    | SOME (#"n", src) => SOME (#"\n", src)
    | SOME (#"v", src) => SOME (#"\v", src)
    | SOME (#"f", src) => SOME (#"\f", src)
    | SOME (#"r", src) => SOME (#"\r", src)
    | (x as SOME (#"\"", src)) => x
    | (x as SOME (#"\\", src)) => x
    | SOME (#"^", src) =>
      (case getc src of
        NONE => NONE
      | SOME (c, src) =>
        if c >= #"@" andalso c <= #"Z"
        then SOME (Prim.i2c (Int.-(Prim.c2i c, 64)), src)
        else NONE)
    | SOME (#"u", src) => 
      let
        fun loop (0, src, result) = SOME (Prim.i2c result, src)
          | loop (n, src, result) =
            case getc src of
              NONE => NONE 
            | SOME (c, src') =>
              let
                val d = fromDigit(c,16)
              in
                if Int.<(d, 0) then NONE
                else loop(Int.-(n,1), src', Int.+(Int.*(result,16), d))
              end
      in
        loop (4, src, 0)
      end

    | (x as SOME (c, src')) =>
      if isSpace c then
      let
        fun whileSpace src =
        case getc src of
          NONE => NONE
        | SOME (#"\\", src) => charscan ML getc src
        | SOME (c, src) =>
          if isSpace c then whileSpace src else NONE
      in
        whileSpace src'
      end

      else 
      let
        val d = fromDigit(c, base)
        fun loop (0, src, result) = SOME (Prim.i2c result, src)
          | loop (n, src, result) =
            case getc src of
              NONE => NONE
            | SOME (c, src') =>
              let
                val d = fromDigit(c, base)
              in
                if Int.<(d,0) then NONE
                else loop(Int.-(n,1), src', Int.+(Int.*(result,base), d))
              end
      in        
        if Int.<(d, 0)
        then
          if ML then NONE
          else (case c of
            #"'" => x
          | #"?" => x
          | _ => NONE)
        else loop (2, src', d)
      end)
      
  | (x as SOME(c, src)) => 
    if c < #" " orelse c >= #"\127" then NONE
    else x

  | NONE => NONE
end

end (* of local open *)

end (* of structure String *)
end (* of structure MLJUtils *)
