/*
 * POLYMORPHISME
 */

/* O. Surcharge */

class C {
  def f(): Int = 42
  def f(x:Boolean): Int = if(x) 42 else 33
}

(new C).f()
(new C).f(false)

// on peut définir des familles de fonctions, paramétrées par des
// types
def swap[A,B](x:A, y:B): (B,A) = (y, x)
// dans le corps de la fonction swap, on ne doit faire aucune
// supposition sur les types A et B; ils sont "abstraits".

// contre-exemple:
def swap[A,B](x:A, y:B): (B,A) = (y+1, x) // ERREUR
  // rien ne dit que le type B aura une méthode "def +(i:Int)"


// utilisation:
swap[Int, Boolean](42, true)
swap[Int, Int](42,33)
swap(42, true) // les arguments de type sont inférés

// Q: combien y a-t-il de fonctions avec la signature de swap?
// R: une seule!

// autre exemple: il n'y a qu'une implémentation *pure* de id:
def id[A](x:A) : A = x
// d'autres possibilités non pures
// println("hello"); x
// throw new MonException()

/* I. Paramétricité */

def f(x:Int): Boolean = ???

// definit une famille de fonctions
// indexées par A et B (n'importe quel type)
def f[A,B](x:A, y:B): (B,A) = (y,x)
f[Int,Bool](1, true)
f(1, true) // les arguments de type sont optionnels

// f doit être une fonction correcte pour *n'importe quel types A et B*
// pas le droit de faire une hypothèse sur les types paramètres:
def f[A,B](x:A, y:B): (B,A) = (y+1,x)	// erreur

// definit une famille de classe
class C[A](x:A) {
  def f: List[A=>Unit] = ???
}

// l'instanciation: (cette fois le paramètre de type obligatoire)
new C[Int]
new C[C[C[C[C[Int]]]]]
new C[Int](4)
new C(4)

/* II. Sous-typage */

// 1/ une classe fille hérite de tous les attribs/meths de sa mère
// 2/ une classe fille peut redéfinir une méthode de sa mère (mot clé
// override obligatoire)
// type = propriété compile-time
// classe = propriété run-time
// Pour les langages dynamiques, type=class (JS, Python)

abstract class C {def p: Unit}
class C1 extends C {def p = println("C1")}
class C2 extends C {def p = println("C2")}

// chaque expression a un type et une classe
// type(s) = propriété compile-time
// classe = propriété run-time

// ex:
val c : C1 = new C1 // type C1, classe C1
val c : C = new C1 // type C, classe C1

// montrons que la classe d'une expression n'est pas déterminable à
// run-time:
(if(Random.nextBoolean())
  new C1() // type C1, classe C1
else
  new C2() // type C2, classe C2
)   // type C, classe C1/C2
.p

// une expression a plusieurs types:
// - son type déclaré
// - tous ses sur-types

// ---> sous-typage T_1 <= T_2
// - si class C1 extends C2 alors C1 <= C2
// - si T_1 <= T_2 et si T_2 <= T_3 alors T_1 <= T_3
// - T <= T

// ---> principe de substitution (Liskov)
// - Si E(x) expression bien typée pour tout x:T
// - Si T <= U
// - alors E(y) expression bien typée pour tout y:U

// ex:
val c:C = new C1()

// (parenthèse: hiérarchie de classe Scala)
val x : Int = 42
val y : Any = x

// Nothing: type des exceptions
val x : Nothing = ???

// Q:
val l : List[C1] = List(new C1, new C1)
val l2 : List[C] = l
val l3 : List[Any] = "hello" :: l

// règle: Si A <= B alors List[A] <= List[B]
// (a.k.a. List est covariant)

// Q: la même chose avec des Array
val a : Array[C1] = Array(new C1, new C1)
val a2 : Array[C] = a

// règle: Array[A] <\= Array[B] si A =\= B
// (a.k.a Array est invariant)

// Q: la même chose avec des fonctions

// les fonctions sont covariantes sur leur retour
val f : () => C1 = () => new C1
val f2 : () => C = f

// ... mais pas sur leurs arguments
val g : C1 => Unit = c => ()
//val g2 : C => Unit = g // echec

// les fonctions sont contravariantes sur leurs arguments
val h : C => Unit = c => ()
val h2 : C1 => Unit = h

// règle:
// - Si A1 <= A2
// - Si B1 <= B2
// Alors (A2 => B1) <= (A1 => B2)

/* III. Variance */

// Le bug de la variance des tableaux en Java:

// String[] a1 = {"abc"};
// Object[] a2 = a1;
// a2[0] = 42;
// String s = a1[0];

// void reverse(Object[] tab);

// Annotation de variance:

class Cell[A] (i:A) { // pas le droit à la covariance(+)
  var c: A = i
}

var c1 : Cell[String] = new Cell[String]("abc")
// var c2 : Cell[Any] = c1 // refusé -> bug évité
// c2.c = 42
// val s : String = c1.c

// Problème:
// Solution: les bornes inférieures sur
// les paramètres de type
class List[+A] {
  def ::[B>:A](x:B) : List[B] = ???
}
