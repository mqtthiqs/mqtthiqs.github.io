// I. Types algébriques simples

// 1. Bool
sealed abstract class Bool
case class True() extends Bool
case class False() extends Bool

object test1 {
  def not(b:Bool) : Bool = b match {case True() => False() case False() => True()}
}

// 5. IOption
sealed abstract class IOption
case class Some(i:Int) extends IOption
case class None() extends IOption

object Map {
  def map(o: IOption, f:Int=>Int): IOption = o match {
    case None() => None()
    case Some(i) => Some(f(i))
  }
}

// 6. IList
sealed abstract class IList {
  def map(f:Int=>Int) : IList = this match {
    case INil() => INil()
    case ICons(x, tl) => ICons(f(x), tl)
  }
  def sum() : Int
}
case class INil() extends IList {
  def sum() = 0
}
case class ICons(x:Int, tl:IList) extends IList {
  def sum() = tl.sum() + x
}

// 8. List
sealed abstract class List[A] {
  def map[B](f:A=>B) : List[B] = this match {
    case Nil() => Nil()
    case Cons(x, tl) => Cons(f(x), tl map f)
  }
  def sum() : Int
}
case class Nil[A]() extends List[A] {
  def sum() = 0
}
case class Cons[A](x:A, tl:List[A]) extends List[A] {
  def sum() = tl.sum() + 1// x
}

// II. Expressions arithmétiques

object Fix {
  def fixpoint[A](f:A=>A, x:A): A = {
    val y = f(x);
    if (x == y) x else fixpoint(f, y)
  }
}

sealed abstract class Expr {
  def depth() : Int
  def simpl() : Expr = this match {
    case BinOp("+", e, Number(0)) => e
    case BinOp("+", Number(0), e) => e
    case BinOp("-", e, Number(0)) => e
    case BinOp("-", Number(0), e) => UnOp("-", e)
    case UnOp("-", UnOp("-", e)) => e
    case BinOp("+", e1, e2) if e1 == e2 => BinOp("*", e1, Number(2))
    case BinOp("-", e1, e2) if e1 == e2 => Number(0)
    case _ => this
  }

  def map1(f: Expr => Expr) = this match {
    case UnOp(op, e) => UnOp(op, f(e))
    case BinOp(op, e1, e2) => BinOp(op, f(e1), f(e2))
    case _ => this
  }

  def deepSimpl(): Expr = this.map1(e => e.deepSimpl())
  def simplify() = Fix.fixpoint[Expr](x => x.deepSimpl(), this)
}
case class Var(x:String) extends Expr { def depth() = 0 }
case class Number(n:Int) extends Expr { def depth() = 0 }
case class UnOp(op:String, arg:Expr) extends Expr { def depth() = arg.depth() + 1 }
case class BinOp(op:String, left:Expr, right:Expr) extends Expr {
  def depth() = (left.depth() max right.depth()) + 1
}

// -2 + x
object test2 {
  val ex : Expr = BinOp("+", UnOp("-", Number(2)), Var("x"))
  ex.depth();
}
