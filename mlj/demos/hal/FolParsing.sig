signature FolParsing =
  sig
  exception SyntaxErr of string
  type token
  val id : token list -> string * token list
  val $  : string -> token list -> string * token list
  val empty : 'a -> 'b list * 'a
  val || : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b
  val !! : ('a -> 'b * 'c) -> 'a -> 'b * 'c
  val -- : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e
  val $-- : string * (token list -> 'a * 'b) -> token list -> 'a * 'b
  val >> : ('a -> 'b * 'c) * ('b -> 'd) -> 'a -> 'd * 'c
  val repeat : ('a -> 'b * 'a) -> 'a -> 'b list * 'a
  val infixes :
      (token list -> 'a * token list) * (string -> int) *
      (string -> 'a -> 'a -> 'a) -> token list -> 'a * token list
  val reader: (token list -> 'a * 'b list) -> string -> 'a
  end;
