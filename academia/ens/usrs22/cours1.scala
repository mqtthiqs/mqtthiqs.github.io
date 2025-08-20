/*
 * Matthias Puech <matthias.puech@lecnam.net>
 * NFP101/USRS22 Sécurité des langages de programmation
 *
 * cedric.cnam.fr/~puechm/ --> Teaching --> USRS22
 *
 * - typage (vérification à la compilation)
 * - style de programmation fonctionnel
 *   ---> encouragé dans certains langages (fonctionnels)
 *
 * - Scala est multi-paradigmes
 *   - fonctionnel
 *   - objets
 *   - procédural
 *
 * 0. Expression vs Instruction
 *
 * - expression:
 *   - qqch qui s'évalue ---> valeur
 *   - construction inductive (lego)
 *   - typée
 *   ex: 2+2; f(x, y^2), f(f(f(f(f(x))))) ...
 * - instruction:
 *   - action qui change l'état de la mémoire
 *   ex: x = 42; printf("coucou"); if (E) {I_1} else {I_2}
 *
 * - dans un langage fonctionnel, on privilégie les expressions
 * - En C:     instructions |-----o----------| expressions
 * - En Scala: instructions |------------o---| expressions
 * - ex: if
 *   - en C: if (E) {I_1} else {I_2} (le if est une instruction)
 *   - en Scala: if (E) E_1 else E_2 (le if est une expression)
 *     - sémantique: si E --> true alors
 *                   "if (E) E_1 else E_2" ---> E_1 sinon
 *                   "if (E) E_1 else E_2" ---> E_2
 *
 * - Style orienté-objet:
 *   - réunir syntaxiquement les données (objets) et les fonctions
 *     sur ces données (méthodes)
 *   - surcharge: méthodes de même nom, distinguées par leur type
 *   - héritage: distinction entre type et classe d'un objet
 */

/*
 * 1. Classes et objets
 */

// déclaration
class C					// classe vide
val x = new C				// instanciation

// champ
class C {
  val a = 42
  val b = 33
}

val x = new C
x.a
x.b
// x.a = 43				// error: reassignment to val

// REM: le type des val est toujours inféré
// REM: on peut le mentionner si on veut:
val x: C = new C

// méthodes
class C {
  val a = 42
  def f(x:Int) = a+x    // REM: pas d'inférence du type des args
}
(new C).f(1)

// expressions arbitraires
class C {
  2+2
  val a = 42
  def f(x:Int) = a+x    // REM: pas d'inférence du type des args
}

// this fait référence à l'objet "courant"
class C {
  2+2
  val a = 42
  def f(x:Int) = this.a+x    // REM: pas d'inférence du type des args
}

// paramètres de classe
class C(x:Int, y:Boolean) {
  val a = x+1
  def f(z:Int) = x+a+z
}
new C(1,2)


/*
 * 2. Opérateurs et méthodes

- en Java: distinction nette entre
  - méthodes (notation pointées)
  - opérateurs (built-in) ex: +-*/
- en Scala: vous êtes libres!

1/ les identifiants peuvent contenir (presque) n'importe quel caractère
 ex:
*/

def +-/!() = ???
// REM: ??? est une constante prédéfinie dont l'évaluation
//      lève une exception

/*
 * 2/ scala offre du sucre syntaxique pour l'appel de méthode:
 * E_1 I E_2 === E_1.I(E_2)  // notation infixe
 * E I === E.I()             // notation postfixe
 * I E === E.I()             // notation préfixe
*/

2+2					// pareil que:
2.+(2)

class C(x:Int, y:Boolean) {
  val a = x+1
  def *(z:Int) = x+a+z
}

// problème de la précédence:
1+2*3
//TODO

// Règle: la précédence est fixée par le 1er caractère de
// l'identifiant:
// */% , +- , :=!, <>, &, ^, |, tous les autres caractères

// problème de l'associativité:
1+2+3					// ===
1.+(2.+(3))
1.+(2).+(3)

// Règle: l'associativité dépend du dernier caractère:
// - associativité gauche ((((1+2)+3)+4)+5) par défaut
// - sauf si le dernier caractère du nom est ":"
//   ---> associativité droite, ex:
1 :: 2 :: 3 :: 4 :: Nil

// REM: on peut utiliser tout identifiant de façon infixe:
1 max 2					// ===
1.max(2)

// Notation préfixe:
// - I E === E.I()
// - on ne peut utiliser que les symboles -+!~
// - et on doit définir la méthode de nom I_unary()
// ex:
class C { def unary_! = 32 }
!(new C)
