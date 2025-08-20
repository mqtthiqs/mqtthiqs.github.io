// COURS 3: Classes mutables

// class Unit a un seul habitant: ()

def inutile(): Unit = ()
  // à quoi ça sert??

// Jusqu'à maintenant, on pouvant appliquer le "Principe de
// Transparence Référentielle": "je peux toujours remplacer une
// expression par sa valeur sans changer le comportement du
// programme".

def x() = cos(0)
x() * x()			   // exactement le même programme que:
1.0 * 1.0			   // même chose que
1.0

// I. Effets de bord

  // ex:
println("hello world")
def x() = {println("calcul"); cos(0)}
x() * x()				// n'est plus la même chose que
1.0 * 1.0

// autre effet de bord:
scala.io.StdIn.readInt()

// ordre supérieur
val f = (x:Int) => x+1 // fonction pure (sans effets de bord)
val g = (x:Int) => {println("calcul"); x+1}
val h = () => println("calcul")

def twice(f:()=>Unit) = {f(); f()}

// exemple de la librairie standard:
List(1,2,3,4).foreach((x:Int) => println(x))
// i.e.
List(1,2,3,4).foreach(x => println(x))
// i.e.
List(1,2,3,4) foreach println

List(1,2,3,4) foreach (x => ())

// appartée:
// class List2[A] extends List[A] {
//   def foreach(f:A=>Unit): Unit = this match {
//       case Nil => ()
//       case x::xs => f(x); xs.foreach(f)
//   }
// }

// II. Variables mutables

// 1. variables mutables
val x = 42 + 42 * 45
x
// x = 43 // interdit!

var x = 33
x
x = 43
x

// 2. champs mutables

class C {
  var x = 0
  def f() = x=x+1
}

// 3. getters/setters

class C {
  var z = 0
  def getY() = z*2
  def setY(n:Int) = y=n/2
}

// a) getter
def x = 2+2
x + x
// est du sucre syntaxique pour
def x() = 2+2
x() + x()

class C {
  var z = 0
  def y = z*2
}

// b) setter
o.x = 42
// est du sucre syntaxique pour
o.x_=(42)

// exemple complet
class Thermometer {
  var celsius = 0.0
  def farenheit = celsius * 9.0/5.0 + 32.0	// getter
  def farenheit_=(f:Double) = celsius = (f-32.0)*5.0/9.0 // setter
}

val t = new Thermometer
t.celsius = 20
t.farenheit // appel au getter
t.farenheit = 80 // appel au setter
t.celsius

// III. Objets singletons

// C'est une classe qui a un seul objet qui porte son nom

object S {
  val v = 44
  def f = println("coucou")
}

// ===

class S {
  val v = 44
  def f = println("coucou")
}
val S = new S
S.v

// à quoi ça sert?
// -> c'est l'equivalent de "static" en Java

// notamment:
object M {
  def main(args: Array[String]) = {
    println("Hello world")
  }
}
