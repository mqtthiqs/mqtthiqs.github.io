// RAPPEL
class C(x:Int) {
  val f = x
  def +(y:Int) = y + this.f
  require(x > 0)
}
// ====
def map[A,B](f:A=>B, xs:List[A]): List[B] = xs match {
  case Nil => Nil
  case x::xs => f(x) :: map(f,xs)
}

// Aujourd'hui: on relie les deux mondes.
// - expliquer le pattern-matching par des constructions de POO
// - définir des nouveaux types algébriques

// 1. Abstract
abstract class C {
  def m(x:Int)
}
// new C    // erreur: pas le droit d'instancier C

// 2. Héritage
class D extend C {
  def m(x:Int) = x * x
}
(new D).m(2)

// 3. Paramètres de type
class C[A] {
  def m(x:A) = (x,x)
}
// (new C[Boolean]).m(2) // mal typé

(new C).m(2) // bien typé, valeur (2,2)

// 4. Case classes
case class C(x:Int, b:Boolean)
// * ajoute un constructeur nommé
   C(2, true)
// * ajoute les arguments comme champs
   (C(2, true)).x
// * définit automatiquement "equals", "toString", "hashCode"
// * "rend possible le pattern-matching"

// 5. Pattern-matching
// Forme générale:
//
// E match {
//   case P_1 => E_1
//   case P_2 => E_2
//   ...
//   case P_n => E_n
// }
//
// "Soit V la valeur de E. Le pattern-matching a comme valeur la
// valeur de E_i si P_i a la forme de (matche) V. (et P_j n'a pas la
// forme de V pour tous les j<i)
//
// Qu'est-ce qu'un pattern P_i?
//
// * "_" (wildcard): matche n'importe quelle valeur
// * "42", "true" (constante): matche exactement cette constante
// * "x", "y" (variable): match n'importe quelle valeur
//                        qui est nommée "x" ou "y" dans E_i
// * "(P_a, P_b, P_c)" (tuple): matche (V_1, V_2, V_3)
//           si P_a matche V_1, P_b matche V_2, P_c matche V_3
// * "C(P_a, P_b)" (case class): matche instance de C si P_a
//                        matche son premier argument et
//                        P_b matche son deuxième argument
// * "P if E" (garde): matche V si P matche V et E s'évalue vers true

def describe(x:Any): String = x match {
  case 3 => "trois"
  case Some(x) => "some "+describe(x)
  case (x:Int) :: xs if x < 4 => "liste non-vide avec "+describe(x)
  case _ => "autre chose"
}

// Exemple complet: les arbres
sealed abstract class Tree
case class Leaf(x: Int) extends Tree
case class Node(left:Tree, right:Tree) extends Tree

val ex: Tree = Node(Leaf(1), Node(Leaf(2), Leaf(3)))

// pattern-matching exhaustif
ex match {
  case Node(Node(_, _), _) => true
  case Node(_, Node(_, _)) => false
  case Leaf(_) => true
  case Node(Leaf(_), Leaf(_)) => false
}

val ex2: Tree = Leaf(1)
// pattern-matching non exhaustif: peut échouer à l'exécution (exception: MatchingError)
ex2 match {
  case Node(Node(_, _), _) => true
  case Node(_, Node(_, _)) => false
}

// case class Troisieme() extends Tree // échoue si Tree est "sealed"
