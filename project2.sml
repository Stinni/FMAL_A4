(* -*- mode: sml-mode; coding: utf-8 -*- *)

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

(* 3c returns the value after a function has been applied to a list of numbers *)
fun reduction f (x::[]) = x
|   reduction f (x::y::xt) = reduction f ((f (x, y))::(xt));

(* 3d returns a partitioned list according to a function *)
fun partition f [] = ([], [])
|   partition f (x::xt) =
    let
    fun filtList f [] lis1 lis2 = (rev(lis1), rev(lis2))
    |   filtList f (x::xt) lis1 lis2 = if (f(x))
            then (filtList f xt (x::lis1) lis2)
            else (filtList f xt lis1 (x::lis2))
    in
    filtList f (x::xt) [] []
end;

(* 4a returns a sorted list where y has been added at the right place *)
fun insert (y:real, []) = y::[]
|   insert (y:real, x::xt) = if y < x then (y::x::xt)
                             else (x::(insert(y, xt)));

(* 4b returns a sorted list by using the insert function *)
fun insertsort [] = []
|   insertsort (x::xt) = (insert(x, (insertsort(xt))));

(* 4c returns the element in the middle of a list *)
fun middle (x::[]) = x
|   middle x =
    let
       val mid = length(x) div 2;
       fun findMid (y::yt) m = if m > 0 then (findMid yt (m - 1))
           else y
    in
       findMid x mid
    end;

(* 4d returns the cartesian product of two lists *)
fun cartesian [] y = []
|   cartesian (x::xt) y =
    let
       fun helper elem [] = []
       |   helper elem (y::yt) = (elem, y)::(helper elem yt)
    in
       (helper x y) @ (cartesian xt y)
    end;

(* 4e  *)
fun mymap f [] = []
|   mymap f (x::xt) = (f x)::(mymap f xt);

(* ---------------- TEST CASES ---------------- *)

zip [1,2,3] ["a","b","c"];
zip [1,2] ["a"];
greaterThan [1,5,3,2,4] 3;
reduction op+ [1,3,5,7,9];
reduction op* [1,3,5,7,9];
partition Char.isLower [#"P",#"a",#"3",#"%",#"b"];

insert (3.3, [1.1, 2.2, 4.4, 5.5]);
insertsort [2.2, 4.4, 5.5, 3.3, 1.1];
middle [1,2,3,4,5];
middle [true,false];
cartesian ["a","b","c"] [1,2];
mymap (fn x => x*x) [1,2,3,4];
