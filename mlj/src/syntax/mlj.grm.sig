signature MLJ_TOKENS =
sig
type ('a,'b) token
type svalue
val UNDERLINEID: (Syntax.symbol) *  'a * 'a -> (svalue,'a) token
val DOTHASHHASH:  'a * 'a -> (svalue,'a) token
val DOTHASH:  'a * 'a -> (svalue,'a) token
val BYTECODE: (Java.OpType) *  'a * 'a -> (svalue,'a) token
val NEWARRAY:  'a * 'a -> (svalue,'a) token
val OVERLOAD:  'a * 'a -> (svalue,'a) token
val VOLATILE:  'a * 'a -> (svalue,'a) token
val TRANSIENT:  'a * 'a -> (svalue,'a) token
val THIS:  'a * 'a -> (svalue,'a) token
val SYNCHRONIZED:  'a * 'a -> (svalue,'a) token
val STATIC:  'a * 'a -> (svalue,'a) token
val SUPER:  'a * 'a -> (svalue,'a) token
val PUTFIELD:  'a * 'a -> (svalue,'a) token
val PUBLIC:  'a * 'a -> (svalue,'a) token
val PROTECTED:  'a * 'a -> (svalue,'a) token
val PRIVATE:  'a * 'a -> (svalue,'a) token
val NEW:  'a * 'a -> (svalue,'a) token
val METHOD:  'a * 'a -> (svalue,'a) token
val INVOKE:  'a * 'a -> (svalue,'a) token
val INTERFACETYPE:  'a * 'a -> (svalue,'a) token
val INSTANCEOF:  'a * 'a -> (svalue,'a) token
val IMPLEMENTS:  'a * 'a -> (svalue,'a) token
val GETFIELD:  'a * 'a -> (svalue,'a) token
val FINAL:  'a * 'a -> (svalue,'a) token
val FIELD:  'a * 'a -> (svalue,'a) token
val EXTENDS:  'a * 'a -> (svalue,'a) token
val CONSTRUCTOR:  'a * 'a -> (svalue,'a) token
val CLASSTYPE:  'a * 'a -> (svalue,'a) token
val CAST:  'a * 'a -> (svalue,'a) token
val ABSTRACT:  'a * 'a -> (svalue,'a) token
val ANDALSO:  'a * 'a -> (svalue,'a) token
val ORELSE:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val RBRACKET:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val LBRACKET:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val COLONGT:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val ASTERISK:  'a * 'a -> (svalue,'a) token
val WITHTYPE:  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val WILD:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val WHERE:  'a * 'a -> (svalue,'a) token
val VAL:  'a * 'a -> (svalue,'a) token
val TYPE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val STRUCTURE:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val SIGNATURE:  'a * 'a -> (svalue,'a) token
val SIG:  'a * 'a -> (svalue,'a) token
val SHARING:  'a * 'a -> (svalue,'a) token
val REC:  'a * 'a -> (svalue,'a) token
val RAISE:  'a * 'a -> (svalue,'a) token
val OPEN:  'a * 'a -> (svalue,'a) token
val OP:  'a * 'a -> (svalue,'a) token
val OF:  'a * 'a -> (svalue,'a) token
val NONFIX:  'a * 'a -> (svalue,'a) token
val LOCAL:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val LAZY:  'a * 'a -> (svalue,'a) token
val INFIXR:  'a * 'a -> (svalue,'a) token
val INFIX:  'a * 'a -> (svalue,'a) token
val INCLUDE:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val HASH:  'a * 'a -> (svalue,'a) token
val HANDLE:  'a * 'a -> (svalue,'a) token
val FUNCTOR:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val FN:  'a * 'a -> (svalue,'a) token
val DARROW:  'a * 'a -> (svalue,'a) token
val DOTS:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val EXCEPTION:  'a * 'a -> (svalue,'a) token
val EQTYPE:  'a * 'a -> (svalue,'a) token
val EQUALOP:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val DOTDOTDOT:  'a * 'a -> (svalue,'a) token
val DATATYPE:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val AS:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val ABSTYPE:  'a * 'a -> (svalue,'a) token
val TYVAR: (Syntax.symbol) *  'a * 'a -> (svalue,'a) token
val SYMBOLIC: (Syntax.symbol) *  'a * 'a -> (svalue,'a) token
val ALPHA: (Syntax.symbol) *  'a * 'a -> (svalue,'a) token
val JAVALONGID: (JavaString.t) *  'a * 'a -> (svalue,'a) token
val JAVAID: (JavaString.t) *  'a * 'a -> (svalue,'a) token
val INTLAB: (string) *  'a * 'a -> (svalue,'a) token
val SCON: (SCon.SCon) *  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val BAD:  'a * 'a -> (svalue,'a) token
end
signature MLJ_LRVALS=
sig
structure Tokens : MLJ_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
