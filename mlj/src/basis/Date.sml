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

structure Date :> DATE =
struct

     datatype weekday
       = Mon
       | Tue
       | Wed
       | Thu
       | Fri
       | Sat
       | Sun
     datatype month
       = Jan
       | Feb
       | Mar
       | Apr
       | May
       | Jun
       | Jul
       | Aug
       | Sep
       | Oct
       | Nov
       | Dec

     fun monthToInt day =
     case day of
       Jan => 0
     | Feb => 1
     | Mar => 2
     | Apr => 3
     | May => 4
     | Jun => 5
     | Jul => 6
     | Aug => 7
     | Sep => 8
     | Oct => 9
     | Nov => 10
     | Dec => 11

     fun intToMonth i =
     case i of
       0 => Jan
     | 1 => Feb 
     | 2 => Mar 
     | 3 => Apr 
     | 4 => May 
     | 5 => Jun 
     | 6 => Jul 
     | 7 => Aug 
     | 8 => Sep 
     | 9 => Oct 
     | 10 => Nov
     | 11 => Dec

     fun intToDay i =
     case i of
       0 => Sun
     | 1 => Mon
     | 2 => Tue
     | 3 => Wed
     | 4 => Thu
     | 5 => Fri
     | 6 => Sat

     type date = "java.util.Date"
     exception Date
     fun date { year, month, day, hour, minute, second, offset } =
       _new date (Java.fromInt (Int.-(year,1900)), 
       Java.fromInt (monthToInt month), 
       Java.fromInt day, Java.fromInt hour, Java.fromInt second)

     fun year (d : date) = Int.+(Java.toInt(_invoke "getYear" d), 1900)
     fun month (d : date) = intToMonth (Java.toInt(_invoke "getMonth" d))
     fun day (d : date) = Java.toInt(_invoke "getDate" d)
     fun hour (d : date) = Java.toInt(_invoke "getHours" d)
     fun minute (d : date) = Java.toInt(_invoke "getMinutes" d)
     fun second (d : date) = Java.toInt(_invoke "getSeconds" d)

     fun weekDay (d : date) = intToDay (Java.toInt(_invoke "getDay" d))

     fun compare (d1 : date, d2 : date) =
     if Java.toBool(_invoke "before" (d1, d2)) then General.LESS
     else if Java.toBool(_invoke "before" (d2, d1)) then General.GREATER
     else General.EQUAL

     fun toString (d : date) = Java.toString (Java.unsafeValOf (
       _invoke "toLocaleString" (d)))

     fun fromString s = Option.SOME (_new date (Java.fromString s))
      
end
