
/* Exercice 1 */

case class Book(title: String, authors: String*)

val books: List[Book] = List(
  Book("Structure and Interpretation of Computer Programs",
       "Abelson, Harold", "Sussman, Gerald J."),
  Book("Principles of Compiler Design",
       "Aho, Alfred", "Ullman, Jeffrey"),
  Book("Programming in Modula-2",
       "Wirth, Niklaus"),
  Book("Elements of ML Programming",
       "Ullman, Jeffrey"),
  Book("The Java Language Specification", "Gosling, James",
       "Joy, Bill", "Steele, Guy", "Bracha, Gilad")
)

// tous les titres de livre
for (b <- books) yield b.title

// liste des livres dont le titre contient "Program"
for (b <- books if b.title contains "Program") yield b

// les titres des livres à un seul auteur
for (b <- books if b.authors.length == 1) yield b.title

// liste de paires (titre, nombre d'auteur)
for (b <- books) yield (b.title, b.authors.length)

// l'ensemble des auteurs de la bibliothèque
for (b <- books; a <- b.authors) yield a

// liste des paires (livre, auteur) si auteur a écrit livre
for (b <- books; a <- b.authors) yield (b.title, a)

// co-auteurs de Guy Steele
for {
  b <- books;
  a <- b.authors if a == "Steele, Guy";
  o <- b.authors if o != "Steele, Guy"
} yield o

// auteurs qui ont écrit plusieurs livres
for {
  b1 <- books;
  b2 <- books if b1 != b2;
  a1 <- b1.authors;
  a2 <- b2.authors if a1 == a2
} yield a1


/* Exercice 2 */

for (x <- List(1,2,3)) yield x + 1
List(1,2,3) map (x => x + 1)

for (i <- 1 to 10) println(i)
1 to 10 foreach (i => println(i))

for (i <- 1 to 10) yield (i, i/2)
1 to 10 map (i => (i, i/2))

for (x <- List(1,2); y <- List(3,4)) yield (x, y)
List(1,2) flatMap (x => for (y <- List(3,4)) yield (x, y))
List(1,2) flatMap (x => List(3,4) map (y => (x, y)))

for (i <- 1 to 10; n=i/2 if n*2==i) yield n
for ((i, n) <- for (i <- 1 to 10) yield (i, i/2); if n*2==i) yield n
for ((i, n) <- 1 to 10 map (i => (i, i/2)); if n*2==i) yield n
for ((i, n) <- 1 to 10 map (i => (i, i/2)) filter {case (i, n) => n*2==i}) yield n
1 to 10 map (i => (i, i/2)) filter {case (i, n) => n*2==i} map {case (i, n) => n}


/* Exercice 3 */

case class Queen(x: Int, y: Int)
type Placement = List[Queen]

def inCheck(q1: Queen, q2: Queen) =
  q1.x == q2.x ||  // same row
  q1.y == q2.y ||  // same column
  (q1.x - q2.x).abs == (q1.y - q2.y).abs // on diagonal

def isSafe(queen: Queen, p: Placement) =
  p forall (q => !inCheck(queen, q))

def queens(n: Int): List[Placement] = {
  def placeQueens(k: Int): List[Placement] =
    if (k == 0) List(List())
    else for {
      queens <- placeQueens(k - 1)
      column <- 1 to n
      queen = Queen(k, column)
	if isSafe(queen, queens)
    } yield queen :: queens

  placeQueens(n)
}

/* Exercice 4 */
def split[A,B](z: Option[(A,B)]): (Option[A],Option[B]) =
  (for ((x,y) <- z) yield x,
   for ((x,y) <- z) yield y)

def combine[A,B](x: Option[A], y: Option[B]): Option[(A,B)] =
  for (a <- x; b <- y) yield (a, b)

def extract[A](z: List[Option[A]]): List[A] =
  for (x <- z; y <- x) yield y

// futures et listes

def map [A,B](f: A => B, z: List[Future[A]]): List[Future[B]] =
  for (x <- z) yield x map f

def filter[A](p: A => Boolean, z: List[Future[A]]): List[Future[Option[A]]] =
  for (x <- z) yield x map (y => if(p(y)) Some(y) else None)

def collect [A](z : List[Future[A]]): Future[List[A]] =
  Future(for (x <- z) yield getResult(x))

// sort

// Identity monad
class Id[A](val x:A) {
  def map[B](f:A=>B): Id[B] = new Id(f(this.x))
  def flatMap[B](f:A=>Id[B]): Id[B] = f(this.x)
}

def split[A](l : List[A], n: Int): (List[A], List[A]) = {
  if (n == 0) (Nil, l)
  else l match {
    case Nil => (Nil, Nil)
      case (h::t) =>
        val (l1, l2) = split(t, n-1)
    (h::l1, l2)
  }
}

def merge(l1 : List[Int], l2 : List[Int]): List[Int] = (l1, l2) match {
  case (Nil, l) => l
  case (l, Nil) => l
  case (h1::t1, h2::t2) =>
    if (h1 > h2) h1::merge(t1, l2)
    else h2::merge(l1, t2)
}

def sort(l : List[Int]): Future[List[Int]] =
  if (l.size <= 1) new Future(l) else {
    val (l1, l2) = split(l, l.size / 2)
    for {r1 <- sortPara(l1)
	 r2 <- sortPara(l2)} yield merge(r1, r2)
  }
