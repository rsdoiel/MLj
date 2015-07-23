signature FolLex =
  sig
  datatype token = Id of string | Key of string
  val scan : string -> token list
  end;
