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

(* CRC:>CRC computes a cyclic redundancy check for the given 
   Word8Vector.vector, in the same format as InfoZip's zip program.
   (We also steal their algorithm).
   *)
structure CRC:>CRC=
struct
(* This is based on crc32.c and crctab.c in the Zip2.1 distribution.  I have
   put both in this directory for convenience *)

   val crctab:Word32.word Vector.vector=
      Vector.fromList [
(* The following vector was taken from crctab.c *)
   0wx00000000, 0wx77073096, 0wxee0e612c, 0wx990951ba, 0wx076dc419,
   0wx706af48f, 0wxe963a535, 0wx9e6495a3, 0wx0edb8832, 0wx79dcb8a4,
   0wxe0d5e91e, 0wx97d2d988, 0wx09b64c2b, 0wx7eb17cbd, 0wxe7b82d07,
   0wx90bf1d91, 0wx1db71064, 0wx6ab020f2, 0wxf3b97148, 0wx84be41de,
   0wx1adad47d, 0wx6ddde4eb, 0wxf4d4b551, 0wx83d385c7, 0wx136c9856,
   0wx646ba8c0, 0wxfd62f97a, 0wx8a65c9ec, 0wx14015c4f, 0wx63066cd9,
   0wxfa0f3d63, 0wx8d080df5, 0wx3b6e20c8, 0wx4c69105e, 0wxd56041e4,
   0wxa2677172, 0wx3c03e4d1, 0wx4b04d447, 0wxd20d85fd, 0wxa50ab56b,
   0wx35b5a8fa, 0wx42b2986c, 0wxdbbbc9d6, 0wxacbcf940, 0wx32d86ce3,
   0wx45df5c75, 0wxdcd60dcf, 0wxabd13d59, 0wx26d930ac, 0wx51de003a,
   0wxc8d75180, 0wxbfd06116, 0wx21b4f4b5, 0wx56b3c423, 0wxcfba9599,
   0wxb8bda50f, 0wx2802b89e, 0wx5f058808, 0wxc60cd9b2, 0wxb10be924,
   0wx2f6f7c87, 0wx58684c11, 0wxc1611dab, 0wxb6662d3d, 0wx76dc4190,
   0wx01db7106, 0wx98d220bc, 0wxefd5102a, 0wx71b18589, 0wx06b6b51f,
   0wx9fbfe4a5, 0wxe8b8d433, 0wx7807c9a2, 0wx0f00f934, 0wx9609a88e,
   0wxe10e9818, 0wx7f6a0dbb, 0wx086d3d2d, 0wx91646c97, 0wxe6635c01,
   0wx6b6b51f4, 0wx1c6c6162, 0wx856530d8, 0wxf262004e, 0wx6c0695ed,
   0wx1b01a57b, 0wx8208f4c1, 0wxf50fc457, 0wx65b0d9c6, 0wx12b7e950,
   0wx8bbeb8ea, 0wxfcb9887c, 0wx62dd1ddf, 0wx15da2d49, 0wx8cd37cf3,
   0wxfbd44c65, 0wx4db26158, 0wx3ab551ce, 0wxa3bc0074, 0wxd4bb30e2,
   0wx4adfa541, 0wx3dd895d7, 0wxa4d1c46d, 0wxd3d6f4fb, 0wx4369e96a,
   0wx346ed9fc, 0wxad678846, 0wxda60b8d0, 0wx44042d73, 0wx33031de5,
   0wxaa0a4c5f, 0wxdd0d7cc9, 0wx5005713c, 0wx270241aa, 0wxbe0b1010,
   0wxc90c2086, 0wx5768b525, 0wx206f85b3, 0wxb966d409, 0wxce61e49f,
   0wx5edef90e, 0wx29d9c998, 0wxb0d09822, 0wxc7d7a8b4, 0wx59b33d17,
   0wx2eb40d81, 0wxb7bd5c3b, 0wxc0ba6cad, 0wxedb88320, 0wx9abfb3b6,
   0wx03b6e20c, 0wx74b1d29a, 0wxead54739, 0wx9dd277af, 0wx04db2615,
   0wx73dc1683, 0wxe3630b12, 0wx94643b84, 0wx0d6d6a3e, 0wx7a6a5aa8,
   0wxe40ecf0b, 0wx9309ff9d, 0wx0a00ae27, 0wx7d079eb1, 0wxf00f9344,
   0wx8708a3d2, 0wx1e01f268, 0wx6906c2fe, 0wxf762575d, 0wx806567cb,
   0wx196c3671, 0wx6e6b06e7, 0wxfed41b76, 0wx89d32be0, 0wx10da7a5a,
   0wx67dd4acc, 0wxf9b9df6f, 0wx8ebeeff9, 0wx17b7be43, 0wx60b08ed5,
   0wxd6d6a3e8, 0wxa1d1937e, 0wx38d8c2c4, 0wx4fdff252, 0wxd1bb67f1,
   0wxa6bc5767, 0wx3fb506dd, 0wx48b2364b, 0wxd80d2bda, 0wxaf0a1b4c,
   0wx36034af6, 0wx41047a60, 0wxdf60efc3, 0wxa867df55, 0wx316e8eef,
   0wx4669be79, 0wxcb61b38c, 0wxbc66831a, 0wx256fd2a0, 0wx5268e236,
   0wxcc0c7795, 0wxbb0b4703, 0wx220216b9, 0wx5505262f, 0wxc5ba3bbe,
   0wxb2bd0b28, 0wx2bb45a92, 0wx5cb36a04, 0wxc2d7ffa7, 0wxb5d0cf31,
   0wx2cd99e8b, 0wx5bdeae1d, 0wx9b64c2b0, 0wxec63f226, 0wx756aa39c,
   0wx026d930a, 0wx9c0906a9, 0wxeb0e363f, 0wx72076785, 0wx05005713,
   0wx95bf4a82, 0wxe2b87a14, 0wx7bb12bae, 0wx0cb61b38, 0wx92d28e9b,
   0wxe5d5be0d, 0wx7cdcefb7, 0wx0bdbdf21, 0wx86d3d2d4, 0wxf1d4e242,
   0wx68ddb3f8, 0wx1fda836e, 0wx81be16cd, 0wxf6b9265b, 0wx6fb077e1,
   0wx18b74777, 0wx88085ae6, 0wxff0f6a70, 0wx66063bca, 0wx11010b5c,
   0wx8f659eff, 0wxf862ae69, 0wx616bffd3, 0wx166ccf45, 0wxa00ae278,
   0wxd70dd2ee, 0wx4e048354, 0wx3903b3c2, 0wxa7672661, 0wxd06016f7,
   0wx4969474d, 0wx3e6e77db, 0wxaed16a4a, 0wxd9d65adc, 0wx40df0b66,
   0wx37d83bf0, 0wxa9bcae53, 0wxdebb9ec5, 0wx47b2cf7f, 0wx30b5ffe9,
   0wxbdbdf21c, 0wxcabac28a, 0wx53b39330, 0wx24b4a3a6, 0wxbad03605,
   0wxcdd70693, 0wx54de5729, 0wx23d967bf, 0wxb3667a2e, 0wxc4614ab8,
   0wx5d681b02, 0wx2a6f2b94, 0wxb40bbe37, 0wxc30c8ea1, 0wx5a05df1b,
   0wx2d02ef8d
      ]

   fun W2w8 w=Word8.fromLargeWord(Word32.toLargeWord w)

   fun crc vec=
   let
      val ones=0wxffffffff 
(* xor'd at the beginning and end of the process *)
   in 
      Word32.xorb(ones,
         Word8Vector.foldl
            (fn(byte,crc)=>
               Word32.xorb(
                  Vector.sub(
                     crctab,
                     Word8.toInt(Word8.xorb(W2w8 crc,byte))
                     ),
                  Word32.>>(crc,0w8)
                  )
               )  
            ones
            vec
         )
   end             
end

