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
(* Parse a top-level command						*)
(*======================================================================*)
structure ParseCommand =
struct

datatype Token = 
  Id of string
| Lit of string
| Plus
| Comma
| Sep
| Question

datatype 'a Result =
  Success of 'a
| Failure of string

open Command

(*----------------------------------------------------------------------*)
(* Translate a special character into a token, if possible.             *)
(*----------------------------------------------------------------------*)
fun toSpecial c =
case c of
  #"+" => SOME Plus
| #"," => SOME Comma
| #"?" => SOME Question
| _    => NONE

(*----------------------------------------------------------------------*)
(* Tokenize a string into a list of lexemes.				*)
(*----------------------------------------------------------------------*)
fun lex s =
let
fun lex' (r, []) = 
    Success (rev r)

  | lex' (r, #"\""::xs) =
    let
      fun loop (s, []) = Failure "unclosed string"
        | loop (s, #"\""::xs') = lex' (Lit s::r, xs')
        | loop (s, xs) =
          case Char.scan (fn [] => NONE | x::xs => SOME (x,xs)) xs of
            NONE => 
            Failure "illegal literal"

          | SOME (c, xs') => 
            loop (s ^ String.str c, xs')
    in
      loop ("", xs)
    end

  | lex' (r, #"\n" :: xs') =
    lex' (Sep::r, xs')

  | lex' (r, #"," :: xs') =
    let
      fun loop [] = Failure "missing values after ,"
        | loop (xs as (x::xs')) = 
          if Char.isSpace x then loop xs' else lex' (Comma::r, xs)
    in
      loop xs'
    end

  | lex' (r, #"-" :: xs') =
    let
      fun loop (s, []) = lex' (Id s::Sep::r, [])
        | loop (s, xs as (x::xs')) =
          if Char.isSpace x orelse isSome (toSpecial x)
          then lex' (Id s::Sep::r, xs)
          else loop (s ^ String.str x, xs')
    in
      loop ("", xs')
    end
    
  | lex' (r, xs as x::xs') = 
    case toSpecial x of
      SOME token => lex' (token::r, xs')
    | NONE =>
      if Char.isSpace x then lex' (r, xs')
      else 
      let
        fun loop (s, []) = lex' (Id s::r, [])
          | loop (s, xs as (x::xs')) =
            if Char.isSpace x orelse isSome (toSpecial x)
            then lex' (Id s::r, xs)
            else loop (s ^ String.str x, xs')
      in
        loop (String.str x, xs')
      end
in
  lex' ([], explode s)
end

fun isIn (c, cs) = List.exists (fn c' => c=c') cs
infix 5 isIn

val maps = [StructureFrom, SignatureFrom, Export, Make]

(*----------------------------------------------------------------------*)
(* What command type is this string?                                    *)
(*----------------------------------------------------------------------*)
fun stringToCommandType s =
case String.map Char.toLower s of
  "bootclasspath"=> SOME BootClassPath
| "cd"           => SOME ChDir
| "classpath"    => SOME ClassPath
| "export"       => SOME Export
| "help"         => SOME Help
| "jvm"          => SOME JVM
| "java10"       => SOME Java10
| "java11"       => SOME Java11
| "log"          => SOME Log
| "make"         => SOME Make
| "quit"         => SOME Quit
| "run"          => SOME Run
| "on"           => SOME On
| "off"          => SOME Off
| "save"         => SOME Save
| "signaturefrom"=> SOME SignatureFrom
| "sourcepath"   => SOME SourcePath
| "structurefrom"=> SOME StructureFrom
| "target"       => SOME Target
| "transforms"   => 
  if !Debug.debugOn then SOME Transforms else NONE
| "localtransforms" => 
  if !Debug.debugOn then SOME LocalTransforms else NONE
| _              => NONE
  
(*----------------------------------------------------------------------*)
(* Parse a sequence of commands.                                        *)
(*----------------------------------------------------------------------*)
fun parseCommands tokens = 
let
  fun parse (r, []) = 
      Success (rev r)

    | parse (r, Sep :: tokens) = 
      parse (r, tokens)

    | parse (r, Lit s :: Sep :: tokens) =
      parse (Command(Exec, NONE, SOME [s])::r, tokens)

    | parse (r, Id s :: tokens) =
      (
        case stringToCommandType s of
        NONE => 
        (case tokens of
          Question :: Sep :: tokens' =>
          parse (Command(Query, NONE, SOME [s])::r, tokens')
          
        | Sep :: tokens' =>
          parse (UserCommand s :: r, tokens')

        | _ => Failure ("no such command: " ^ s))

      | SOME c =>
        if c isIn maps then
        let
          fun error () = 
            if c = Make orelse c = Export
            then Failure 
              "expected exports of form [+] <longid> [<classname>],...,<longid> [<classname>] [+]"
            else Failure "expected mapping of form [+] <id> <file>,...,<id> <file> [+]"
          fun rest (ext, pairs, tokens) =
              case tokens of
                Comma :: tokens => 
                parseMap (ext, pairs, tokens)

              | Sep :: tokens => 
                parse (Map(c, ext, SOME (rev pairs))::r, tokens)

              | Plus :: Sep :: tokens =>
                if isSome ext 
                then Failure "arguments cannot be both prefixed and postfixed"
                else parse (Map(c, SOME Prefix, SOME (rev pairs))::r, tokens)

              | _ => 
                Failure "expected , or newline"

          and parseMap (ext, pairs, Id x::Id y::tokens) =
              rest (ext, (x,y)::pairs, tokens)

            | parseMap (ext, pairs, Id x::Lit y::tokens) =
              rest (ext, (x,y)::pairs, tokens)

            | parseMap (ext, pairs, 
              Id x::(tokens as (Comma | Sep | Plus)::_)) =
              if c=Make orelse c=Export then rest(ext, (x,"")::pairs, tokens)
              else error ()              

            | parseMap (ext, pairs, tokens) =
              error ()
        in
          case tokens of
            Plus :: tokens => 
            parseMap (SOME Postfix, [], tokens)

          | Question :: Sep :: tokens => 
            parse (Map(c, NONE, NONE)::r, tokens)

          | Sep :: tokens =>
            parse (Map(c, NONE, SOME [])::r, tokens)

          | _ =>
            parseMap (NONE, [], tokens)
        end else 

        let
          fun parseNames (ext, names, Id x::tokens) =
              parseNames (ext, x::names, tokens)

            | parseNames (ext, names, Lit x::tokens) =
              parseNames (ext, x::names, tokens)

            | parseNames (ext, names, Plus :: Sep :: tokens) =
              if isSome ext 
              then Failure "arguments cannot be both prefixed and postfixed"
              else parse (Command(c, SOME Prefix, SOME (rev names))::r,tokens)

            | parseNames (ext, names, Sep :: tokens) = 
              parse (Command(c, ext, SOME (rev names))::r, tokens)
     
            | parseNames _ =
              Failure "expected arguments to command or newline"
        in
          case tokens of
            Plus :: tokens => 
            parseNames (SOME Postfix, [], tokens)

          | Question :: Sep :: tokens => 
            parse (Command(c, NONE, NONE)::r, tokens)

          | _ => 
            parseNames (NONE, [], tokens)
        end
      )

    | parse _ = Failure "syntax error in command"
in
  parse ([], tokens)
end

fun parse s = 
case lex s of
  Success tokens => parseCommands tokens
| Failure s => Failure s

end
