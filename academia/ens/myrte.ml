(*
 * Myrte: un langage d'expressions arithmétiques
 * =============================================
 *
 * Support pour le cours MV6 (Université Paris Diderot)
 *
 * Définition, interprétation, machine virtuelle, compilation
 *
 * @author Matthias Puech <puech@pps.univ-paris-diderot.fr>
 * @year 2013
 *)


(**************************************)
(*     expressions et interprète      *)
(**************************************)

type value =
  | Int of int
  | Bool of bool

type binop = Add | Eq | And

type expr =
  | Const of value
  | Binop of binop * expr * expr

let ex1 =
  let deux = Const (Int 2) in
  Binop (Eq, Binop (Add, deux, deux), Const (Int 4))

let ex2 = Binop (Eq, Const (Int 2), Const (Bool true))

let rec interp : expr -> value = function
  | Const v -> v
  | Binop (b, e1, e2) -> match b, interp e1, interp e2 with
    | Add, Int i, Int j -> Int (i + j)
    | Eq, Int i, Int j -> Bool (i = j)
    | And, Bool i, Bool j -> Bool (i && j)
    | _ -> failwith "ill-formed expression"

(* Tests *)
let () =
  assert (interp ex1 = Bool true);
  assert (try ignore (interp ex2); false with Failure _ -> true)


(**************************************)
(*             typage                 *)
(**************************************)

type tp = TInt | TBool

let rec infer : expr -> tp = function
  | Const (Int _) -> TInt
  | Const (Bool _) -> TBool
  | Binop (b, e1, e2) -> match b, infer e1, infer e2 with
    | Add, TInt, TInt -> TInt
    | Eq, TInt, TInt -> TBool
    | And, TBool, TBool -> TBool
    | _ -> failwith "expression mal typee"

let check e = try ignore (infer e); true with Failure _ -> false


(**************************************)
(*          machine à A-pile          *)
(**************************************)

type instr = Push | Consti of int | Addi | Eqi | Andi

type state = {
  mutable acc: int;
  code: instr array;
  mutable pc: int; (* indice de l’instruction courante dans code *)
  stack: int array;
  mutable sp: int; (* indice du sommet de la pile dans stack *)
}

let machine s = while s.pc < Array.length s.code do
    begin match s.code.(s.pc) with
    | Consti n ->
      s.acc <- n
    | Push ->
      s.sp <- s.sp + 1;
      s.stack.(s.sp) <- s.acc
    | Addi ->
      s.acc <- s.stack.(s.sp) + s.acc;
      s.sp <- s.sp-1
    | Andi ->
      s.acc <- s.stack.(s.sp) * s.acc;
      s.sp <- s.sp-1
    | Eqi ->
      s.acc <- if s.stack.(s.sp) = s.acc then 1 else 0;
      s.sp <- s.sp-1 end;
  s.pc <- s.pc + 1
  done; s

let init c =
  { code = c; stack = Array.make 1000 42;
    pc = 0; sp = -1; acc = 52 }

(* Tests *)
let () =
  assert ((machine (init [|Consti 2; Push|])).acc = 2);
  assert ((machine (init [|Consti 2; Push; Consti 2; Addi|])).acc = 4);
  assert ((machine (init [|Consti 2; Push; Addi|])).acc = 4);
  assert ((machine (init [|Push; Addi|])).acc = 104);
  assert (try ignore (machine (init [|Addi|])); false with _ -> true );
  assert (try ignore (machine (init (Array.make 1001 Push))); false with _ -> true)


(**************************************)
(*          Compilation               *)
(**************************************)

let repr : value -> int = function
  | Bool true -> 1
  | Bool false -> 0
  | Int i -> i

let op = function Add -> Addi | And -> Andi | Eq -> Eqi

let eval c =
  let s = machine
    { code = Array.of_list c;
      stack = Array.make 1000 42;
      pc = 0;
      sp = -1;
      acc = 52 } in
  s.acc

(* compil met la repr. de la valeur dans l’accum. et restaure la pile *)
let rec compil : expr -> instr list = function
  | Const v -> [Consti (repr v)]
  | Binop (o, e1, e2) ->
    compil e1 @ [Push] @
      compil e2 @ [op o]

(* version par passage d’accumulateur: liste renvoyee renversee *)
let rec compil l : expr -> instr list = function
  | Const v -> Consti (repr v) :: l
  | Binop (o, e1, e2) ->
    let l = Push :: compil l e1 in
    op o :: compil l e2

let compil e = List.rev (compil [] e)

let () =
  assert (eval (compil ex1) = repr (interp ex1))


(**************************************)
(*           Assemblage               *)
(**************************************)

(* Codage:
 * une instruction par octet:
 * - bits 0-2: l'opcode de l'instruction
 * - bits 3-7: vides, sauf pour Consti -> l'entier
 * Opcodes:
 * - Push -> 0
 * - Addi -> 1
 * - Eqi -> 2
 * - Andi -> 3
 * - Consti -> 4
 *)

let assemble (p : instr array) : string =
  let s = String.make (Array.length p) 'z' in
  for i = 0 to Array.length p - 1 do
    s.[i] <- match p.(i) with
    | Push -> Char.chr 0
    | Addi -> Char.chr 1
    | Eqi -> Char.chr 2
    | Andi -> Char.chr 3
    | Consti n -> assert (n < 32); Char.chr (4 + n lsl 3);
  done; s

let disassemble (s : string) : instr array =
  let p = Array.make (String.length s) Push in
  for i = 0 to String.length s - 1 do
    p.(i) <- match Char.code s.[i] with
    | 0 -> Push
    | 1 -> Addi
    | 2 -> Eqi
    | 3 -> Andi
    | n when (n mod 8 = 4) -> Consti (n lsr 3)
    | _ -> failwith "invalid byte-code"
  done; p

(* test *)
let p1 = Array.of_list (compil ex1)
let p2 = Array.of_list (compil ex1)
let () =
  assert (disassemble (assemble p1) = p1);
  assert (disassemble (assemble p2) = p2);
