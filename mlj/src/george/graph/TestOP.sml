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

(* Test for OrthogonalPartition *)
open Graph
val V=Vector.tabulate(100,newNode o ignore)
fun v i=Vector.sub(V,i)
structure IP=IntIntervalPartialIntSet
structure OP=OrthogonalPartition(IP)

val nlist=
  [{node=v 1,label=(2,{from=1,to=2})},
   {node=v 2,label=(1,{from=0,to=2})},
   {node=v 3,label=(3,{from=1,to=3})},
   {node=v 4,label=(2,{from=2,to=3})},
   {node=v 5,label=(1,{from=1,to=3})},
   {node=v 6,label=(3,{from=0,to=1})},
   {node=v 7,label=(3,{from=2,to=4})}]

val alist=[
   ({from=v 1,to=v 3},1.0),
   ({from=v 1,to=v 4},1.4),
   ({from=v 1,to=v 2},1.0),
   ({from=v 2,to=v 5},1.0),
   ({from=v 4,to=v 6},1.0),
   ({from=v 4,to=v 7},2.0)
    ]
val pg=pre{nodes=nlist,arcs=alist}
val OPed=OP.prefind(pg,fn x=>x)
