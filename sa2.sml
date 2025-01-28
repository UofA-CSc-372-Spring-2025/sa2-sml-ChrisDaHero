(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Chris Peña                             *)
(* Time spent on SA2: 2 hours *)

(* Collaborators and references: ChatGPT
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)
(* Problem A will till you whether or not a list is empty and will do so in constant time and without using Standard
Ml Library *)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

(** Additional Unit Test for Problem A **)
val () =
    Unit.checkExpectWith Bool.toString "mynull [1, 2, 3, 4] should be false"
    (fn () => mynull [1, 2, 3, 4])
    false

(** One more Unit test for the road **)
val () =
    Unit.checkExpectWith Bool.toString "mynull [1] should be false"
    (fn () => mynull [1])
    false



(**** Problem B ****)
(* Problem B tasks with returning false if if the list is empty or if the first letter is not a vowel *)
fun firstVowel []      = false
    | firstVowel (x::_) =
        let val vowel = Char.toLower x
        in List.exists (fn vowel => vowel = lowercase) [#"a", #"e", #"i", #"o", #"u"]
        end; (** Fix above later **)


val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true
*)

(**** Problem C ****)
(*
fun reverse xs = xs

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]
*)
(**** Problem D ****)
(*
fun minlist _ = 0

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0
*)
(**** Problem E ****)
(*
exception Mismatch

fun zip _ = []
*)
(**** Problem F ****)
(*
fun concat xs = xs
*)
(**** Problem G ****)
(*
fun isDigit _    = false;
*)
(**** Problem H ****)
(*
fun isAlpha c = false
*)
(**** Problem I ****)
(*
fun svgCircle (cx, cy, r, fill) = "NOT IMPLEMENTED YET"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
*)
(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
