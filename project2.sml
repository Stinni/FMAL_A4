(* -*- mode: sml-mode; coding: utf-8; -*- *)

(*
Student: Kristinn Heidar Freysteinsson
Kt.: 011081-3209
Username: kristinnf13
Email: kristinnf13@ru.is
*)

(* 3a returns a list of pairs *)
fun zip [] y = []
|   zip x [] = []
|   zip (x::xt) (y::yt) = ((x, y)::(zip xt yt));

(* 3b returns a list of elements from a list that're greater than k *)
fun greaterThan [] k = []
|   greaterThan (x::xt) k = if x > k then x::(greaterThan xt k)
                          else (greaterThan xt k);

