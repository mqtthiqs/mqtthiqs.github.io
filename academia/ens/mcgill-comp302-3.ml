(* COMP302, McGill, Sep. 9-11, 2014

   Matthias Puech <puech@cs.mcgill.ca>
*)

(* Summary of last week:
   Algebraic data types
   ==================== *)

(* Declaration: *)
(* 1. enumeration *)
type suit = Spade | Heart | Diamond | Club
type figure = King | Queen | Ace (* 3 constructors *)

(* 2. type synonyms *)
type card = suit * figure (* type synonym *)

(* 3. recursive type *)
type hand = Empty | Card of card * hand

(* 4. polymorphic type *)
type 'a list' = Nil | Cons of 'a * 'a list'

(* value construction (a.k.a constructors) *)
let h = Card ((Spade, King), Card ((Diamond, Queen), Empty))

(* value observation (a.k.a pattern matching) *)
let b = match h with
  | Card ((Spade, _), _) -> true
  | _ -> false

(* tuples *)
let e = (1, true, "hello")
let _ = match e with
  | (a, b, c) -> a

(* Tutorial: toplevel, compilation
   =============================== *)

(*
- 2 kinds of toplevel phrases:
  - type/exceptions
  - values declaration
- in Emacs:
  - send it to OCaml interpreter: C-c C-e
*)


(* 1. type/exception declaration *)
exception Unbound_input

(* 2. value declaration *)
let answer = 42
let four = 2 + 2  (* computation *)
let (_ : int) = 2 + 2  (* anonymous & type annotation *)
(* functions *)
let double x = x * 2
let rec fact x = if x=0 then 1 else x * fact (x-1)
let _ = fact 7

(* compiler: [ocamlc] (C-c C-c in Emacs)
   useful for:
   - syntax/type check
*)

(* program does nothing, unless... *)
let _ = print_string "hello\n"

(* Show type at point:
   - "ocamlc -annot file.ml"
   - C-c C-t in Emacs *)

(* More Algebraic Data Types
   ========================= *)

(* ** Aside: built-in list notation: *)
let e1 = [] (* = Empty *)
let e2 = 3 :: e1 (* = Cons (1, e) *)
let e3 = 1 :: 2 :: e2 (* = Cons (2, Cons (3, e2)) *)
let e4 = [1;2;3]      (* = 1 :: 2 :: 3 :: [] *)

(* ** Recursive functions on lists: *)
let rec length xs = match xs with
  | [] -> 0
  | _ :: xs -> 1 + length xs

let _ = length e4

let rec append xs ys = match xs with
  | [] -> ys
  | x :: xs -> x :: append xs ys

let _ = append [1;2;3] [4;5;6]

let rec rev xs = match xs with
  | [] -> []
  | x :: xs -> rev xs @ [x]

let rev' xs =
  let rec rev_a acc xs = match xs with
    | [] -> acc
    | x :: xs -> rev_a (x::acc) xs in
  rev_a xs []

(* rem: pre-defined as "@" *)

(* Deep pattern-matching:
   e.g. group by pairs: [1;2;3;4] -> [(1,2); (3,4)] *)

let rec group xs = match xs with
  | x :: y :: xs -> (x, y) :: group xs
  | [] -> []
  (* deep pattern matching *)
  | [0] -> []
  (* additional case after seeing error *)
  | [_] -> raise Unbound_input

let _ = group [1;2;3;4]


(* ** INTERLUDE: Core & Standard library *)

(* ** Binary trees *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let ex = Node (Leaf, 42, Node (Leaf, 2, Leaf))

let rec max_depth t = match t with
  | Leaf -> 0
  | Node (l, _, r) -> max (max_depth l) (max_depth r)

(* ** Option *)

type 'a option = None | Some of 'a

let _ = Some "bla"
let _ = None

(* they are a type-safe alternative to exceptions: *)
let rec last xs = match xs with
  | [x] -> Some x
  | x :: xs -> last xs
  | [] -> None
