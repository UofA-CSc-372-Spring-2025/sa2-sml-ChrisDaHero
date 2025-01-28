(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Chris Peña                             *)
(* Time spent on SA2: 2 hours *)

(* Collaborators and references: ChatGPT, cs.cornell.edu on Folding and tail recursion
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
fun firstVowel [] = false
    | firstVowel (x::_) =   (* Future note this pattern is for the first element and can be modified for certain positions in list *)
        let val lowercase = Char.toLower x  (* Can lower characters like in C *)
        in List.exists (fn vowel => vowel = lowercase) [#"a", #"e", #"i", #"o", #"u"]
        end;


val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

(* Three more unit tests one empty one with capital letters and one with a consonant *)
val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'kck' should be false"
    (fn () => firstVowel [#"k",#"c",#"k"])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ACK' should be true"
    (fn () => firstVowel [#"A",#"C",#"K"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel '' should be False"
    (fn () => firstVowel [])
    false



(**** Problem C ****)
(* Problem C will use foldl to reverse a list i.e [1,2] becomes [2,1]*)
fun reverse xs = foldl List.:: [] xs  (* foldl really reversed a list in one line *)

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

(* two more tests will do for Problem C *)

val () =
    Unit.checkExpectWith (Unit.listString Int.toString)
    "reverse [2, 1, 2, 1] should be [1, 2, 1, 2]"
    (fn () => reverse [2, 1, 2, 1])
    [1, 2, 1, 2]

val () =
    Unit.checkExpectWith (Unit.listString Int.toString)
    "reverse [] should be []"
    (fn () => reverse [])
    []

(**** Problem D ****)
(* Will return the smallest element of a nonempty list of integers using foldl and not recursion *)
(* list empty throw an exception by raise Match *)
fun minlist [] = raise Match (* if empty raise match *)
    | minlist (x::xs) = foldl Int.min x xs;  (* fold l goes through whole list and Int.min it all *)

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

(**** Problem E ****)
(* function zip takes a pair of lists of equal length and returns equivalent list of pairs *)
(* if pairs don't match raise exception Mismatch which must be defined no if and no Standard basis library *)
exception Mismatch;
fun zip ([], []) = []
    | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
    | zip (_, _) = raise Mismatch;

(* need to make testcases for this function *)
val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([1, 2], [3, 4]) should be [(1, 3), (2, 4)]"
  (fn () => zip ([1, 2], [3, 4]))
  [(1, 3), (2, 4)];


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
