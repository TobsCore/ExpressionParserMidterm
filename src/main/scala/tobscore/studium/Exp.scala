import scala.annotation.tailrec

case class Fraction(numerator: Int, denominator: Int) {
  def simplify(): Option[Fraction] = {
    if (denominator == 0) {
      return None
    }
    // Calculate greatest common divisor. In order to get correct results, the positive values of the numerator and denominator are used.
    val gcd = greatestCommonDivisor(Math.abs(numerator), Math.abs(denominator))
    Some(Fraction(numerator / gcd, denominator / gcd))
  }

  @tailrec
  final def greatestCommonDivisor(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => greatestCommonDivisor(b, a % b)
  }
}

sealed trait Exp {
  def +(e: Exp): Plus = Plus(this, e)

  def -(e: Exp): Minus = Minus(this, e)

  def *(e: Exp): Multiply = Multiply(this, e)

  def /(e: Exp): Divide = Divide(this, e)
}

case class Val(v: Fraction) extends Exp {
  def this(numerator: Int, denominator: Int) = this(Fraction(numerator, denominator))
}

object Val {
  def apply(numerator: Int, denominator: Int): Val = new Val(numerator, denominator)
}

case class Plus(e1: Exp, e2: Exp) extends Exp {
}

// Question 3
case class Minus(e1: Exp, e2: Exp) extends Exp {
}

case class Multiply(e1: Exp, e2: Exp) extends Exp {
}

case class Divide(e1: Exp, e2: Exp) extends Exp {
}

object ExpParser {
  // Question 5
  def pretty(e: Exp): String = e match {
    case Val(fraction) => s"(${fraction.numerator}/${fraction.denominator})"
    case Plus(e1, e2) => s"(${pretty(e1)}+${pretty(e2)})"
    case Minus(e1, e2) => s"(${pretty(e1)}-${pretty(e2)})"
    case Multiply(e1, e2) => s"(${pretty(e1)}*${pretty(e2)})"
    case Divide(e1, e2) => s"(${pretty(e1)}//${pretty(e2)})"
  }

  // Question 6
  def eval(e: Exp): Option[Fraction] = e match {
    case Val(fraction) => fraction.simplify()
    case Plus(e1, e2) =>
      val frac1 = eval(e1)
      val frac2 = eval(e2)

      if (frac1.isDefined && frac2.isDefined) {
        val newDenominator = frac1.get.denominator * frac2.get.denominator
        val newNumeratorFrac1 = frac1.get.numerator * frac2.get.denominator
        val newNumeratorFrac2 = frac2.get.numerator * frac1.get.denominator

        Fraction(newNumeratorFrac1 + newNumeratorFrac2, newDenominator).simplify()
      } else {
        None
      }
    case Minus(e1, e2) =>
      val frac1 = eval(e1)
      val frac2 = eval(e2)

      if (frac1.isDefined && frac2.isDefined) {
        val newDenominator = frac1.get.denominator * frac2.get.denominator
        val newNumeratorFrac1 = frac1.get.numerator * frac2.get.denominator
        val newNumeratorFrac2 = frac2.get.numerator * frac1.get.denominator

        Fraction(newNumeratorFrac1 - newNumeratorFrac2, newDenominator).simplify()

      } else {
        None
      }
    case Multiply(e1, e2) =>
      val frac1 = eval(e1)
      val frac2 = eval(e2)

      if (frac1.isDefined && frac2.isDefined) {
        Fraction(frac1.get.numerator * frac2.get.numerator, frac1.get.denominator * frac2.get.denominator).simplify()
      } else {
        None
      }
    case Divide(e1, e2) =>
      val frac1 = eval(e1)
      val frac2 = eval(e2)

      if (frac1.isDefined && frac2.isDefined) {
        Fraction(frac1.get.numerator * frac2.get.denominator, frac1.get.denominator * frac2.get.numerator).simplify()
      } else {
        None
      }
  }
}