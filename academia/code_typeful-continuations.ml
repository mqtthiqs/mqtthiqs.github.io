
(* {VALUE} *)
type o and 'a vl = VFun : ('a vl -> ('b vl -> o) -> o) -> ('a -> 'b) vl
(* {END} *)

let ex : 'a. ('a -> 'a) vl = VFun (fun x k -> k x)
(* {VALUE_EX} *)
let ex : 'a 'b. (('a -> 'b) -> 'a -> 'b) vl =
  VFun (fun (VFun f) k -> k (VFun (fun x k -> f x (fun v -> k v))))
(* {END} *)

(* {VARIABLES} *)
type 'a x = X of 'a vl
type 'a v = V of 'a vl
type 'a k = K of ('a vl -> o)
(* {END} *)

(* {SOURCE} *)
type 'a m =
  | MLam : ('a x -> 'b m) -> ('a -> 'b) m
  | MApp : ('a -> 'b) m * 'a m -> 'b m
  | MVar : 'a x -> 'a m
  | MLet : 'a m * ('a x -> 'c m) -> 'c m
(* {END} *)

(* {SOURCE_EX} *)
let ex : 'a 'b. (('a -> 'b) -> 'a -> 'b) m =
  MLam (fun f -> MLam (fun x -> MApp (MVar f, MVar x)))
(* {END} *)
module One_pass = struct

(* {ONEPASS_LANG} *)
type s =                  (* Serious terms *)
  | SRet : 'a k * 'a t -> s
  | SBind : ('a -> 'b) t * 'a t * ('b v -> s) -> s
  | SLet : 'a t * ('a x -> s) -> s
and 'a t =                 (* Trivial terms *)
  | TLam : ('a x -> 'b k -> s) -> ('a -> 'b) t
  | TVar : 'a x -> 'a t
  | TVal : 'a v -> 'a t
type 'a p =                (* Programs *)
  | PInit : ('a k -> s) -> 'a p
(* {END} *)

(* {ONEPASS_LANG_EX} *)
let ex : 'a 'b. (('a -> 'b) -> 'a -> 'b) p = PInit (fun k ->
    SRet (k, TLam (fun f k -> SRet (k, TLam (fun x k ->
        SBind (TVar f, TVar x, fun v -> SRet (k, TVal v)))))))
(* {END} *)

(* {ONEPASS} *)
let rec cps : type a. a m -> (a t -> s) -> s = function
  | MVar x -> fun c -> c (TVar x)
  | MLam m -> fun c -> c (TLam (fun x k -> cps (m x) (fun v -> SRet (k, v))))
  | MApp (m, n) -> fun c -> cps m (fun t -> cps n (fun u -> SBind (t, u, fun v -> c (TVal v))))
  | MLet (m, f) -> fun c -> cps m (fun m -> SLet (m, fun x -> cps (f x) (fun n -> c n)))

let cps : 'a. 'a m -> 'a p = fun m -> PInit (fun k -> cps m (fun v -> SRet (k, v)))
(* {END} *)

(* "the only observations we can make on programs is termination" (Kennedy) *)
(* {ONEPASS_EVAL} *)
let rec exec : s -> o = function
  | SLet (t, f) -> exec (f (X (eval t)))
  | SRet (K c, t) -> c (eval t)
  | SBind (t, u, s) -> match eval t with
    | VFun f -> f (eval u) (fun v -> exec (s (V v)))
and eval : type a. a t -> a vl = function
  | TVar (X x) | TVal (V x) -> x
  | TLam f -> VFun (fun x k -> exec (f (X x) (K k)))
(* {END} *)

(* but we can "escape" the continuation *)
(* {ONEPASS_ESCAPE} *)
let eval : type a. a p -> a vl = fun (PInit s) ->
  let module M = struct exception E of a vl end in
  try ignore (exec (s (K (fun x -> raise (M.E x))))); assert false
  with M.E x -> x
(* {END} *)

end

(* Just tail calls, for the record (not included in the paper) *)
module Tail_call = struct

type s =
  | SRet : 'a k * 'a t -> s
  | SComp : u -> s

and u =
  | UBind : ('a -> 'b) t * 'a t * 'b c -> u
  | ULet : 'a t * ('a x -> s) -> u

and 'a t =
  | TLam : ('a x -> 'b k -> s) -> ('a -> 'b) t
  | TVar : 'a x -> 'a t
  | TVal : 'a v -> 'a t

and 'a c =
  | Cont : ('a v -> u) -> 'a c
  | Contvar : 'a k -> 'a c

type 'a p = PInit : ('a k -> s) -> 'a p

let rec cps : type a. a m -> (a t -> u) -> u = function
  | MVar x -> fun c -> c (TVar x)
  | MApp (m, n) -> fun c -> cps m (fun t -> cps n (fun u ->
      UBind (t, u, Cont (fun v -> c (TVal v)))))
  | MLam m -> fun c -> c (TLam (fun x k -> cpst (m x) k))
  | MLet (m, f) -> fun c -> cps m (fun t ->
      ULet (t, fun x -> SComp (cps (f x) (fun u -> c u))))

(* called on terms in tail position *)
and cpst : type a. a m -> a k -> s = function
  | MVar x -> fun k -> SRet (k, TVar x)
  | MLam m -> fun k -> SRet (k, TLam (fun x k -> cpst (m x) k))
  | MLet (m, f) -> fun k -> SComp (cps m (fun m ->
      ULet (m, fun x -> cpst (f x) k)))
  | MApp (m, n) -> fun k -> SComp (cps m (fun m -> cps n (fun n ->
      UBind (m, n, Contvar k))))

let cps : type a. a m -> a p = fun m -> PInit (fun k -> cpst m k)
(* {END} *)

end

(* {BETA_IDENT} *)
type 'a i = IVar of 'a x | IVal of 'a v (* Identifiers *)
(* {END} *)

module Beta_redexes = struct

(* {BETA_SYNTAX} *)
type s = (* Serious terms  *)
  | SBind : ('a -> 'b) i * 'a t * ('b v -> s) -> s
  | SRet : 'a k * 'a t -> s
  | SLet : 'a t * ('a x -> s) -> s
and 'a t = (* Trivial terms *)
  | TLam : ('a x -> 'b k -> s) -> ('a -> 'b) t
  | TIdent : 'a i -> 'a t
type 'a p = PInit of ('a k -> s) (* Programs *)
(* {END} *)

(* two purposes:
   - indicates the position of the term (inherited attribute):
     unknown or functional;
   - relates the type 'a of a term to the type 'x of the result of
     translating it. *)
(* {BETA_LEVEL} *)
type ('a, 'x) l =
  | Base : ('a, 'a t) l
  | Arr : ('b, 'x) l -> ('a -> 'b, 'a t -> ('x -> s) -> s) l
(* {END} *)

(* {BETA_EX} *)
let ex : 'a 'b 'c. ('a -> 'b -> 'c, 'a t -> (('b t -> ('c t -> s) -> s) -> s) -> s) l =
  Arr (Arr Base)
(* {END} *)

(* {BETA_PSI} *)
let rec psi : type a x. (a, x) l -> a i -> x = function
  | Base -> fun i -> TIdent i
  | Arr l -> fun i t c -> SBind (i, t, fun v -> c (psi l (IVal v)))
(* {END} *)

(* {BETA_CPS} *)
let rec cps : type a x. (a, x) l -> a m -> (x -> s) -> s = fun l -> function
  | MVar x -> fun c -> c (psi l (IVar x))
  | MApp (m, n) -> fun c -> cps (Arr l) m (fun m -> cps Base n (fun n -> m n (fun a -> c a)))
  | MLet (m, n) -> fun c -> cps Base m (fun t -> SLet (t, fun x -> cps l (n x) (fun u -> c u)))
  | MLam m -> fun c -> match l with
    | Base -> c (TLam (fun x k -> cps Base (m x) (fun m -> SRet (k, m))))
    | Arr l -> c (fun t k -> SLet (t, fun x -> cps l (m x) k))
(* {END} *)
(* {BETA_CPS_MAIN} *)
let cps : 'a. 'a m -> 'a p = fun m -> PInit (fun k -> cps Base m (fun x -> SRet (k, x)))
(* {END} *)

(* {BETA_EVAL} *)
let rec exec : s -> o = function
  | SLet (t, f) -> exec (f (X (eval t)))
  | SRet (K c, t) -> c (eval t)
  (* difference: one recursive call less to eval, since it is inlined
     in the pattern *)
  | SBind ((IVar (X (VFun f)) | IVal (V (VFun f))), t, s) ->
    f (eval t) (fun v -> exec (s (V v)))
and eval : type a. a t -> a vl = function
  | TIdent (IVar (X x) | IVal (V x)) -> x
  | TLam f -> VFun (fun x k -> exec (f (X x) (K k)))
(* {END} *)

end

module Final = struct

(* {FINAL_SYNTAX} *)
type s = SRet : 'a k * 'a t -> s | SComp : u -> s
and u = UBind : ('a -> 'b) i * 'a t * 'b c -> u | ULet : 'a t * ('a x -> s) -> u
and 'a c = CCont : ('a v -> u) -> 'a c | CTail : 'a k -> 'a c
and 'a t = TIdent : 'a i -> 'a t | TLam : ('a x -> 'b k -> s) -> ('a -> 'b) t
type 'a p = PInit of ('a k -> s)
(* {END} *)

(* {FINAL_L} *)
type ('a, 'x) l =
  | Base : ('a, 'a t) l
  (* modification here: changed s into u *)
  | Arr : ('b, 'x) l -> ('a -> 'b, 'a t -> ('x -> u) -> u) l
  (* new: tail position indicator. This marks a term in functional position of a tail application. *)
  | Tail : ('a -> 'b, 'a t -> 'b k -> u) l
(* {END} *)

(* {FINAL_PSI} *)
(* same as before *)
let rec psi : type a x. (a, x) l -> a i -> x = function
  | Base -> fun i -> TIdent i
  | Arr l -> fun i t c -> UBind (i, t, CCont (fun v -> c (psi l (IVal v))))
  (* new case: where we generate a tail bind *)
  | Tail -> fun i t k -> UBind (i, t, CTail k)
(* {END} *)

(* {FINAL_CPS} *)
(* all this is as before (except that it returns a u *)
let rec cps : type a x. (a, x) l -> a m -> (x -> u) -> u = fun l -> function
  | MVar x -> fun c -> c (psi l (IVar x))
  | MApp (m, n) -> fun c -> cps (Arr l) m (fun m -> cps Base n (fun n -> m n (fun a -> c a)))
  | MLet (m, n) -> fun c -> cps Base m (fun t ->
      ULet (t, fun x -> SComp (cps l (n x) (fun u -> c u))))
  | MLam m -> fun c -> match l with
    (* changes start here: *)
    | Base -> c (TLam (fun x k -> cpst (m x) k)) (* subterm is tail *)
    | Arr l -> c (fun t k -> ULet (t, fun x -> SComp (cps l (m x) k)))
    (* new: the lambda is part of a nested redex in tail position so we produce a let with a body in tail position *)
    | Tail -> c (fun t k -> ULet (t, fun x -> cpst (m x) k))

(* identical to the previous section, except... *)
and cpst : type a. a m -> a k -> s = function
  | MVar x -> fun k -> SRet (k, TIdent (IVar x))
  | MLam m -> fun k -> SRet (k, TLam (fun x k -> cpst (m x) k))
  | MLet (m, f) -> fun k -> SComp (cps Base m (fun m -> (* m is not in tail position *)
      ULet (m, fun x -> cpst (f x) k)))
  | MApp (m, n) -> fun k -> SComp (cps Tail m (fun m -> (* m is a function in tail position *)
      cps Base n (fun n -> m n k)))
(* {END} *)

(* {FINAL_TOPLEVEL} *)
let cps : 'a. 'a m -> 'a p = fun m -> PInit (fun k -> cpst m k)
(* {END} *)

(* {FINAL_EVAL} *)
let rec exec : s -> o = function
  | SRet (K c, t) -> c (eval t)
  | SComp c -> compute c
and compute : u -> o = function
  | ULet (t, f) -> exec (f (X (eval t)))
  | UBind ((IVar (X (VFun f)) | IVal (V (VFun f))), t, c) -> f (eval t) (continue c)
and continue : type a. a c -> a vl -> o = function
  | CCont s -> fun v -> compute (s (V v))
  | CTail (K c) -> c
and eval : type a. a t -> a vl = function
  | TIdent (IVar (X x) | IVal (V x)) -> x
  | TLam f -> VFun (fun x k -> exec (f (X x) (K k)))
(* {END} *)

(* {FINAL_EVAL_INLINED} *)
(* the same with continue inlined: *)
let rec exec : s -> o = function
  | SRet (K c, t) -> c (eval t)
  | SComp c -> compute c
and compute : u -> o = function
  | ULet (t, f) -> exec (f (X (eval t)))
  | UBind ((IVar (X (VFun f)) | IVal (V (VFun f))), t, CTail (K c)) ->
    f (eval t) c
  | UBind ((IVar (X (VFun f)) | IVal (V (VFun f))), t, CCont s) ->
    f (eval t) (fun v -> compute (s (V v)))
and eval : type a. a t -> a vl = function
  | TIdent (IVar (X x) | IVal (V x)) -> x
  | TLam f -> VFun (fun x k -> exec (f (X x) (K k)))
(* {END} *)

end
