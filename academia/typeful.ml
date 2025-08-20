(* Typeful Normalization by Evaluation
   ===================================

   Authors: Olivier Danvy & Chantal Keller & Matthias Puech
   Date: September 8, 2014

*)


(* Deep vs shallow representation of terms *)

module DeepShallow = struct

  (* Deep terms *)

  type tp = Base                              (* Some base type *)
          | Arr of tp * tp


  type tm = Var of x
          | Lam of (x -> tm)
          | App of tm * tm
  and x (* The variable namespace, uninstantiated for now *)

  let exD =
    Lam (fun f -> Lam (fun x -> App (Var f, Var x)))

  let exDT =
    Arr (Arr (Base, Base), Arr (Base, Base))

  (* Shallow terms *)

  type base (* Some base type, uninstantiated for now *)

  type vl = VFun of (vl -> vl)
          | VBase of base

  (* let exS = VFun (fun (VFun f) -> VFun (fun x -> f x)) *)
end


(* GADTs in OCaml on an example *)

module Ex_GADT = struct
  (* Example of printf
     Translation of http://okmij.org/ftp/typed-formatting/PrintScan.hs
     format specification:
     - 'a is the return type
     - 'b is the type constructed for the rest of the arguments
  *)

  type ('a, 'b) directive =
    | Lit : string -> ('a, 'a) directive
    | String : ('a, string -> 'a) directive
    | Int : ('a, int -> 'a) directive
    | Seq : ('b, 'c) directive * ('a, 'b) directive -> ('a, 'c) directive

  let (^^) a b = Seq (a, b) and (!) x = Lit x and d = Int and s = String

  let ex_directive : ('a, int -> string -> int -> string -> 'a) directive =
    d ^^ !" * " ^^ s ^^ !" = " ^^ d ^^ !" in " ^^ s

  let string_of_string x = x

  let rec kprintf : type a b. (a, b) directive -> (string -> a) -> b =
    function
    | Lit s -> fun k -> k s
    | Int -> fun k x -> k (string_of_int x)
    | String -> fun k x -> k (string_of_string x)
    | Seq (f,g) -> fun k -> kprintf f (fun v -> kprintf g (fun w -> k (v^w)))

  let printf dir = kprintf dir print_string

  (* prints "6 * 9 = 42 in base 13" *)
  let () = printf ex_directive 6 "9" 42 "base 13"
end


(* Intro: untyped, standard NbE *)

module Untyped = struct

  (* Intermediate language of values *)

  type vl = VFun of (vl -> vl)
          | VBase of base

  and base = Atom of at

  and x = vl


  (* Target language: β-normal λ-terms *)

  and nf = NLam of (y -> nf)
         | NAt of at

  and at = AApp of at * nf
         | AVar of y

  and y


  (* Source language: λ-terms *)

  type tm = Var of x
          | Lam of (x -> tm)
          | App of tm * tm


  (* Evaluation function: from source to intermediate *)

  let rec eval : tm -> vl = function
    | Var x -> x
    | Lam f -> VFun (fun x -> eval (f x))
    | App (m, n) -> match eval m with
      | VFun f -> f (eval n)
      | VBase _ -> failwith "Unidentified Functional Object"


  (* Simple types *)

  type tp =
    | Base
    | Arr of tp * tp


  (* reify and reflect: from intermediate to target *)

  let rec reify : tp -> vl -> nf = fun a v -> match a, v with
    | Arr (a, b), VFun f -> NLam (fun x -> reify b (f (reflect a (AVar x))))
    | Base, VBase v -> let (Atom r) = v in NAt r
    | _ -> failwith "type mismatch"

  and reflect : tp -> at -> vl = fun a r -> match a with
    | Arr (a, b) -> VFun (fun x -> reflect b (AApp (r, reify a x)))
    | Base -> VBase (Atom r)

  let nbe : tp -> tm -> nf = fun a m -> reify a (eval m)

end


(* Typeful NbE *)

module Typeful = struct

  (* Intermediate language of values, typed *)

  type 'a vl = VFun : ('a vl -> 'b vl) -> ('a -> 'b) vl
             | VBase : base -> base vl

  and base = Atom of base at


  (* Typed target language: β-normal, η-long λ-terms *)

  and 'a nf = NLam : ('a y -> 'b nf) -> ('a -> 'b) nf
            | NAt : base at -> base nf

  and 'a at = AApp : ('a -> 'b) at * 'a nf -> 'b at
            | AVar : 'a y -> 'a at

  and 'a y

  and 'a x = 'a vl


  (* Typed source language *)

  type 'a tm = Lam : ('a x -> 'b tm) -> ('a -> 'b) tm
             | App : ('a -> 'b) tm * 'a tm -> 'b tm
             | Var : 'a x -> 'a tm


  (* Evaluation function: from source to intermediate *)

  let rec eval : type a. a tm -> a vl = function
    | Var x -> x
    | Lam f -> VFun (fun x -> eval (f x))
    | App (m, n) -> let VFun f = eval m in f (eval n)


  (* Simple types, annotated by OCaml types *)

  type 'a tp = Base : base tp
             | Arr : 'a tp * 'b tp -> ('a -> 'b) tp


  (* reify and reflect: from intermediate to target *)

  let rec reify : type a. a tp -> a vl -> a nf = fun a v -> match a, v with
    | Arr (a, b), VFun f -> NLam (fun x -> reify b (f (reflect a (AVar x))))
    | Base, VBase v -> let (Atom r) = v in NAt r

  and reflect : type a. a tp -> a at -> a vl = fun a r -> match a with
    | Arr (a, b) -> VFun (fun x -> reflect b (AApp (r, reify a x)))
    | Base -> VBase (Atom r)

  let nbe : type a. a tp -> a tm -> a nf = fun a m -> reify a (eval m)
end


(* Example: Printf *)

module Printf = struct

  type 'a nf =
    | NLam : ('a x -> 'b nf) -> ('a -> 'b) nf
    | NAt : 'a base at -> 'a base nf

  and 'a at =
    | APrim : 'a -> 'a base at
    | AConcat : string base at * string base at -> string base at
    | AStringOfInt : int base at -> string base at
    | AApp : ('a -> 'b) at * 'a nf -> 'b at
    | AVar : 'a x -> 'a at

  and 'a x

  (* all this does not change: *)
  and 'a base = Atom of 'a base at

  type 'a tp =
    | Base : 'a base tp
    | Arr : 'a tp * 'b tp -> ('a -> 'b) tp

  type 'a vl =
    | VFun : ('a vl -> 'b vl) -> ('a -> 'b) vl
    | VBase : 'a base -> 'a base vl

  let rec reify : type a. a tp -> a vl -> a nf = fun a v -> match a, v with
    | Arr (a, b), VFun f -> NLam (fun x -> reify b (f (reflect a (AVar x))))
    | Base, VBase v -> let (Atom r) = v in NAt r

  and reflect : type a. a tp -> a at -> a vl = fun a r -> match a with
    | Arr (a, b) -> VFun (fun x -> reflect b (AApp (r, reify a x)))
    | Base -> VBase (Atom r)

  type int_ = int base at
  type string_ = string base at
  let string_of_string i = APrim i
  let string_of_int x = AStringOfInt x
  let (^) s t = AConcat (s, t)

  (* note: The only things changing are:
     - int -> int_, string -> string_
     - string_of_string for Lit
  *)
  type ('a, 'b) directive =
    | Int : ('a, int_ -> 'a) directive
    | String : ('a, string_ -> 'a) directive
    | Lit : string -> ('a, 'a) directive
    | Seq : ('b, 'c) directive * ('a, 'b) directive -> ('a, 'c) directive

  let rec kprintf : type a b. (a, b) directive -> (string_ -> a) -> b = function
    | Int -> fun k x -> k (string_of_int x)
    | String -> fun k x -> k x
    | Lit s -> fun k -> k (string_of_string s)
    | Seq (f, g) -> fun k -> kprintf f (fun v -> kprintf g (fun w -> k (v ^ w)))

  let printf f = kprintf f (fun x -> x)

  let (^^) a b = Seq (a, b) and (!) x = Lit x and d = Int and s = String
  let ex_directive : ('a, int_ -> string_ -> int_ -> string_ -> 'a) directive =
    d ^^ !" * " ^^ s ^^ !" = " ^^ d ^^ !" in " ^^ s

  let f0 =
    fun x y z t ->
      printf ex_directive x y z t

  let f1 = Pervasives.(
      fun x y z t ->
        string_of_int x ^ " * " ^ y ^ " = " ^ string_of_int z ^ " in " ^ t
    )

  let residual =
    let box f = VFun (fun (VBase (Atom r)) -> f r) in
    reify (Arr (Base, Arr (Base, Arr (Base, (Arr (Base, Base))))))
      (box (fun x -> box (fun y -> box (fun z -> box (fun t ->
           reflect Base (printf ex_directive x y z t))))))

end


(* NbE in CPS, call-by-value *)

module CPS_CBV = struct

  type 'a k and 'a v and 'a y


  (* Intermediate language of values in CPS, typed *)

  type 'a vl = VFun : ('a vl -> 'b md) -> ('a -> 'b) vl
             | VBase : base -> base vl
             | VBool : bool -> bool vl
  and 'a md = ('a vl -> o) -> o
  and 'a x = 'a vl


  (* Typed target language: β-normal, η-long λ-terms
     (modulo commuting conversions) *)

  and o = SIf : bool at * o * o -> o
        | SRet : 'a k * 'a nf -> o
        | SBind : ('a -> 'b) at * 'a nf * ('b v -> o) -> o

  and 'a nf = NBool : bool -> bool nf
            | NLam : ('a y -> 'b k -> o) -> ('a -> 'b) nf
            | NAt : base at -> base nf

  and 'a at = AVar of 'a y
            | AVal of 'a v

  and base = Atom of base at


  (* Two examples of values in CPS *)

  let id : type a. (a -> a) vl = VFun (fun x k -> k x)

  let app : type a b. ((a -> b) -> a -> b) vl =
    VFun (fun (VFun f) k -> k (VFun (fun x k -> f x (fun v -> k v))))


  (* Typed source language, with Booleans and call/cc *)

  type 'a tm =
    | If : bool tm * 'a tm * 'a tm -> 'a tm
    | CC : (('a -> 'b) -> 'a) tm -> 'a tm
    | Lam : ('a x -> 'b tm) -> ('a -> 'b) tm
    | App : ('a -> 'b) tm * 'a tm -> 'b tm
    | Var : 'a x -> 'a tm


  (* Example of a source term *)

  let tt = Var (VBool true)


  (* Evaluation function in CPS CBV: from source to intermediate *)

  let rec eval : type a. a tm -> a md = function
    | Var x -> fun c -> c x
    | Lam f -> fun c -> c (VFun (fun x k -> eval (f x) k))
    | App (m, n) -> fun c -> eval m (fun (VFun f) -> eval n (fun n -> f n c))
    | If (b, m, n) -> fun c -> eval b (fun (VBool b) ->
        if b then eval m c else eval n c)
    | CC m -> fun c -> eval m (fun (VFun f) -> f (VFun (fun x _ -> c x)) c)

  let ex : type a. (a -> a) tm = Lam (fun x -> If (Var (VBool true), Var x, Var x))


  (* Annotated simple types, enriched with Booleans *)

  type 'a tp = Bool : bool tp
             | Base : base tp
             | Arr : 'a tp * 'b tp -> ('a -> 'b) tp


  (* reify and reflect in CPS CBV: from intermediate to target *)

  let rec reify : type a. a tp -> a vl -> (a nf -> o) -> o =
    fun a v -> match a, v with
      | Arr (a, b), VFun f -> fun c -> c (NLam (fun x k ->
          reflect a (AVar x) (fun x -> f x (fun v ->
              reify b v (fun v -> SRet (k, v))))))
      | Base, VBase (Atom r) -> fun c -> c (NAt r)
      | Bool, VBool b -> fun c -> c (NBool b)

  and reflect : type a. a tp -> a at -> (a vl -> o) -> o =
    fun a x -> match a, x with
      | Arr (a, b), f -> fun c -> c (VFun (fun x k ->
          reify a x (fun x -> SBind (f, x, fun v ->
              reflect b (AVal v) (fun v -> k v)))))
      | Base, r -> fun c -> c (VBase (Atom r))
      | Bool, b -> fun c -> SIf (b, c (VBool true), c (VBool false))


  (* The final NbE function *)

  let nbe : type a. a tp -> a tm -> (a nf -> o) -> o =
    fun a m k -> eval m (fun m -> reify a m k)

  type 'a c = Init of ('a k -> o)

  let nbe : type a. a tp -> a tm -> a c = fun a m ->
    Init (fun k -> eval m (fun m -> reify a m (fun v -> SRet (k, v))))

end


(* Same development in call-by-name *)

module CPS_CBN = struct

  type 'a k and 'a v


  (* Intermediate language of values in CPS, typed *)

  type 'a vl =
    | VFun : ('a md -> 'b md) -> ('a -> 'b) vl
    | VBase : base -> base vl
    | VBool : bool -> bool vl

  and 'a md = ('a vl -> o) -> o


  (* Typed target language: β-normal, η-long λ-terms
     (modulo commuting conversions) *)

  and o = SRet : 'a k * 'a nf -> o
        | SBind : ('a -> 'b) at * ('a k -> o) * ('b v -> o) -> o
        | SVar : 'a x * ('a v -> o) -> o
        | SIf : bool at * o * o -> o

  and 'a nf = NAt : base at -> base nf
            | NLam : ('a x -> 'b k -> o) -> ('a -> 'b) nf
            | NBool : bool -> bool nf

  and 'a at = AVal : 'a v -> 'a at

  and base = Atom of base at

  and 'a x = 'a md


  (* An examples of a value in CPS *)

  let id = VFun (fun x k -> x k)


  (* The same source language as before *)

  type 'a tm =
    | Lam : ('a x -> 'b tm) -> ('a -> 'b) tm
    | App : ('a -> 'b) tm * 'a tm -> 'b tm
    | Var : 'a x -> 'a tm
    | Bool : bool -> bool tm
    | If : bool tm * 'a tm * 'a tm -> 'a tm
    | CC : (('a -> 'b) -> 'a) tm -> 'a tm


  (* An example of a term in this language *)

  let ex : type a. (a -> a) tm = Lam (fun x -> If (Bool true, Var x, Var x))


  (* Evaluation function in CPS CBN: from source to intermediate *)

  let rec eval : type a. a tm -> a md = function
    | Var x -> fun c -> x c
    | Lam f -> fun c -> c (VFun (fun x k -> eval (f x) k))
    | App (m, n) -> fun c -> eval m (fun (VFun f) -> f (eval n) c)
    | Bool b -> fun c -> c (VBool b)
    | If (b, m, n) -> fun c -> eval b
        (function VBool true -> eval m c | VBool false -> eval n c)
    | CC m -> fun c -> eval m (fun (VFun f) ->
        f (fun k -> k (VFun (fun x _ -> x c))) c)


  (* The same simple types as before *)

  type 'a tp =
    | Bool : bool tp
    | Base : base tp
    | Arr : 'a tp * 'b tp -> ('a -> 'b) tp


  (* reify and reflect in CPS CBN: from intermediate to target *)

  let rec reify : type a. a tp -> a vl -> (a nf -> o) -> o =
    fun a v -> match a, v with
      | Arr (a, b), VFun f -> fun c -> c (NLam (fun x k ->
          f (fun k -> SVar (x, fun v -> reflect a (AVal v) k))
            (fun v -> reify b v (fun v -> SRet (k, v)))))
      | Bool, VBool b -> fun c -> c (NBool b)
      | Base, VBase (Atom r) -> fun c -> c (NAt r)

  and reflect : type a. a tp -> a at -> (a vl -> o) -> o =
    fun a x -> match a, x with
      | Arr (a, b), f -> fun c -> c (VFun (fun x k ->
          SBind (f, (fun k -> x (fun v -> reify a v (fun v -> SRet (k, v)))),
                 (fun v -> reflect b (AVal v) k))))
      | Bool, b -> fun c -> SIf (b, c (VBool true), c (VBool false))
      | Base, r -> fun c -> c (VBase (Atom r))


  (* The final NbE function *)

  let nbe : type a. a tp -> a tm -> (a nf -> o) -> o =
    fun a m k -> eval m (fun m -> reify a m k)

end

(* Annex: From dB to HOAS *)

module DBtoHOAS = struct

  type 'a x

  type 'a hoas =
    | Var : 'a x -> 'a hoas
    | Lam : ('a x -> 'b hoas) -> ('a -> 'b) hoas
    | App : ('a -> 'b) hoas * 'a hoas -> 'b hoas

  type ('g,'a) var =
    | Top : ('g*('a hoas),'a) var
    | Pop : ('g,'a) var -> ('g*('b hoas),'a) var

  type ('g,'a) db =
    | Var : ('g,'a) var -> ('g,'a) db
    | Lam : ('g*('a hoas),'b) db -> ('g,'a -> 'b) db
    | App : ('g,'a -> 'b) db * ('g,'a) db -> ('g,'b) db

  let rec coerce_var : type g a. (g,a) var -> g -> a hoas = function
    | Top -> fun (_,r) -> r
    | Pop v -> fun (g,_) -> coerce_var v g

  let rec coerce : type g a. (g,a) db -> g -> a hoas = function
    | Var v -> coerce_var v
    | Lam f -> fun g -> Lam (fun x -> coerce f (g,Var x))
    | App (f,u) -> fun g -> App (coerce f g, coerce u g)

end
