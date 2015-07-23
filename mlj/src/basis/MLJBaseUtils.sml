structure MLJBaseUtils =
struct

fun radixToBase StringCvt.BIN = 2
  | radixToBase StringCvt.OCT = 8
  | radixToBase StringCvt.DEC = 10
  | radixToBase StringCvt.HEX = 16

fun fromDigit (c, base) = java.lang.Character.digit (c, base)

end
