// Exercice 1:

def apply[A,B](f:A=>B, x:A): B = f(x)
def compose[A,B,C](f:A=>B, g:B=>C): A=>C = x => g(f(x))
def curry[A,B,C](f:(A,B)=>C): A=>(B=>C) = x => y => f(x,y)
def uncurry[A,B,C](f:A=>B=>C): (A,B)=>C = (x,y) => f(x)(y)
def distrib[A,B,C](f:A=>B, g:A=>C): A=>(B,C) = x => (f(x), g(x))
def swap[A,B](x:(A,B)): (B,A) = x match {case (x,y) => (y,x)}
def triplet[A,B,C](x:((A,B),C)): (A,B,C) = x match {case ((x,y),z) => (x,y,z)}

def apply2[A](f:A=>A, x:A): A = f(f(f(f(x))))
def swap2[A](x:(A,A)): (A,A) = x match {case (x,y) => (x,x)}

class Stack[+A](s:List[A]) {
  private val st: List[A] = s
  def push[B>:A](x:B): Stack[B] = new Stack(x :: st)
  def pop : (A, Stack[A]) = st match {
    case x :: xs => (x, new Stack(xs))
    case Nil => throw new Exception()
  }
}

val s : Stack[Int] = (new Stack(List())).push(1).push(2).push(3)
val t : Stack[Any] = s.push("hello")

// Exercice 2:

// 1.
def f(x:Int): String = ???
def g(x:String): Int = ???
def h(x:Nothing): Any = if((new scala.util.Random()).nextBoolean) f(x) else g(x)

// 2.
val l = List(1,2,3)
val l2 = "hello" :: l
val a1: Array[Int] = Array(1,2,3)
//val a2: Array[Any] = a1

// 3.
def reverse(xs:List[Any]) = xs.reverse
reverse(l2)
def reverse2(xs:Array[Any]) = xs.reverse
//reverse2(a1)

// 4. (voir au dessus)

// Exercice 3:

object Typed {
  sealed abstract class BinOperator[O1,O2,R] {
    def eval(x:O1, y:O2): R
  }
  case object Add extends BinOperator[Int, Int, Int] {
    def eval(x:Int, y:Int) = x + y
  }
  case object And extends BinOperator[Boolean, Boolean, Boolean] {
    def eval(x:Boolean, y:Boolean) = x && y
  }
  case object Le extends BinOperator[Int, Int, Boolean] {
    def eval(x:Int, y:Int) = x <= y
  }
  sealed abstract class UnOperator[O, R] { def eval(x:O) : R }
  case object Minus extends UnOperator[Int, Int] {
    def eval(x:Int) = -x
  }
  case object Not extends UnOperator[Boolean, Boolean] {
    def eval(x:Boolean) = !x
  }

  sealed abstract class Expr[A] {
    def eval : A
  }
  case class Num(n:Int) extends Expr[Int] {
    def eval : Int = n
  }
  case class Bool(b:Boolean) extends Expr[Boolean] {
    def eval : Boolean = b
  }
  case class UnOp[O,R](op:UnOperator[O,R], arg:Expr[O]) extends Expr[R] {
    def eval = op.eval(arg.eval)
  }
  case class BinOp[O1, O2, R](op:BinOperator[O1, O2, R], left:Expr[O1], right:Expr[O2]) extends Expr[R] {
    def eval = op.eval(left.eval, right.eval)
  }

  val ex1 : Expr[Boolean] = BinOp(Le, BinOp(Add, Num(2), Num(2)), Num(4))  // 2+2 <= 4
  // val ex2 : Expr[???] = BinOp(And, BinOp(Add, Bool(true), Num(3)), Num(8))  // (true + 3) && 8

  val ex3 : Boolean = ex1.eval
}
