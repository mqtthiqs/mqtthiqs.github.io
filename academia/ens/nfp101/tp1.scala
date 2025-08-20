
class Rational(n:Int, d:Int) {
  require (d != 0)
  val p = pgcd(n,d)
  val num = n / p
  val den = d / p
  override def toString() =
    num.toString()++"/"++den.toString()

  private def pgcd(a: Int, b: Int): Int =
    if (b==0) a else pgcd(b, a % b)

  def +(that: Rational) =
    new Rational(this.num * that.den + this.den * that.num, this.den * that.den)

  def -(that: Rational) =
    new Rational(this.num * that.den - this.den * that.num, this.den * that.den)

  def unary_-() =
    new Rational(-this.num, this.den)

  def +(n: Int): Rational = this + new Rational(n, 1)

  def *(that: Rational) =
    new Rational(this.num * that.num, this.den * that.den)
}

implicit def intToRational(x:Int) = new Rational(x, 1)

val x = new Rational(3,6)
x + x

val one = new Rational(1,1)
val half = new Rational(1,2)
val third = new Rational(1,3)
-half + one
one + third * half
(one + third) * half
one - one - one
half + 1
1 + half
