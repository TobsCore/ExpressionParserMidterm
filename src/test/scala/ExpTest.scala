import org.scalatest.{BeforeAndAfter, FunSuite}

class ExpTest extends FunSuite {

  val e1 = Val(Fraction(1, 2))
  val e2 = Plus(Val(Fraction(3, 4)), e1)
  val e3 = Minus(Multiply(Plus(Val(Fraction(3, 4)), Val(Fraction(1, 2))), Val(Fraction(1, 2))), Divide(Val(Fraction(10, 13)), e2))
  val e4 = Divide(Val(Fraction(5, 6)), Minus(e1, Val(Fraction(2, 4))))
  val e5 = Multiply(Val(Fraction(10, 13)), Val(Fraction(12, 14)))

  test("Prettify e1") {
    assert(ExpParser.pretty(e1) == "(1/2)")
  }

  test("Prettify e2") {
    assert(ExpParser.pretty(e2) == "((3/4)+(1/2))")
  }

  test("Prettify e3") {
    assert(ExpParser.pretty(e3) == "((((3/4)+(1/2))*(1/2))-((10/13)//((3/4)+(1/2))))")
  }

  test("Prettify e4") {
    assert(ExpParser.pretty(e4) == "((5/6)//((1/2)-(2/4)))")
  }

  test("Prettify e5") {
    assert(ExpParser.pretty(e5) == "((10/13)*(12/14))")
  }

  test("Simplify 2/4") {
    assert(Fraction(2, 4).simplify().get == Fraction(1, 2))
  }

  test("Simplify 3/4") {
    assert(Fraction(3, 4).simplify().get == Fraction(3, 4))
  }

  test("Simplify -2/4") {
    assert(Fraction(-2, 4).simplify().get == Fraction(-1, 2))
  }

  test("Simplify 2/0") {
    assert(Fraction(2, 0).simplify().isEmpty)
  }

  test("Eval 3/3") {
    assert(ExpParser.eval(Val(Fraction(3, 3))).get == Fraction(1, 1))
  }

  test("Eval 2/0") {
    assert(ExpParser.eval(Val(Fraction(3, 0))).isEmpty)
  }

  test("Eval (1/2) + (1/2)") {
    val exp = Plus(Val(Fraction(1, 2)), Val(Fraction(1, 2)))
    assert(ExpParser.eval(exp).get == Fraction(1, 1))
  }

  test("Eval (2/2) + (3/2)") {
    val exp = Plus(Val(Fraction(2, 2)), Val(Fraction(3, 2)))
    assert(ExpParser.eval(exp).get == Fraction(5, 2))
  }

  test("Eval (-1/2) + (1,2)") {
    val exp = Plus(Val(Fraction(-1, 2)), Val(Fraction(1, 2)))
    // Since all the numbers are encoded to be a fraction, 0 is encoded as (0,1)
    assert(ExpParser.eval(exp).get == Fraction(0, 1))
  }

  test("Eval (1/1) - (1/2)") {
    val exp = Minus(Val(Fraction(1, 1)), Val(Fraction(1, 2)))
    assert(ExpParser.eval(exp).get == Fraction(1, 2))
  }

  test("Eval (1/2) - (1/2)") {
    val exp = Minus(Val(Fraction(1, 2)), Val(Fraction(1, 2)))
    assert(ExpParser.eval(exp).get == Fraction(0, 1))
  }

  test("Eval (1/2) - (3,2)") {
    val exp = Minus(Val(Fraction(1, 2)), Val(Fraction(3, 2)))
    assert(ExpParser.eval(exp).get == Fraction(-1, 1))
  }

  test("Eval (3/4) - (-2, 4)") {
    val exp = Minus(Val(Fraction(3, 4)), Val(Fraction(-2, 4)))
    assert(ExpParser.eval(exp).get == Fraction(5, 4))
  }

  test("Eval (1/2) * (1/2)") {
    val exp = Multiply(Val(Fraction(1, 2)), Val(Fraction(1, 2)))
    assert(ExpParser.eval(exp).get == Fraction(1, 4))
  }

  test("Eval (1/0) * (2/1)") {
    val exp = Multiply(Val(Fraction(1, 0)), Val(Fraction(2, 1)))
    assert(ExpParser.eval(exp).isEmpty)
  }

  test("Eval (1/2) * (2/1)") {
    val exp = Multiply(Val(Fraction(1, 2)), Val(Fraction(2, 1)))
    assert(ExpParser.eval(exp).get == Fraction(1, 1))
  }

  test("Eval (-1/2) * (3/4)") {
    val exp = Multiply(Val(Fraction(-1, 2)), Val(Fraction(3, 4)))
    assert(ExpParser.eval(exp).get == Fraction(-3, 8))
  }

  test("Eval (1/2) / (1/2)") {
    val exp = Divide(Val(Fraction(1, 2)), Val(Fraction(1, 2)))
    assert(ExpParser.eval(exp).get == Fraction(1, 1))
  }

  test("Eval (1/2) / (2/1)") {
    val exp = Divide(Val(Fraction(1, 2)), Val(Fraction(2, 1)))
    assert(ExpParser.eval(exp).get == Fraction(1, 4))
  }

  test("Eval (3/4) / (9/3)") {
    val exp = Divide(Val(Fraction(3, 4)), Val(Fraction(9, 3)))
    assert(ExpParser.eval(exp).get == Fraction(1, 4))
  }

  test("Eval (1/2) * (3,2) + (5,4) / (7, 3)") {
    val exp = Plus(Multiply(Val(Fraction(1,2)), Val(Fraction(3,2))), Divide(Val(Fraction(5, 4)), Val(Fraction(7,3))))
    assert(ExpParser.eval(exp).get == Fraction(9, 7))
  }

  test("Adding two Vals by calling + method") {
    val exp = Val(Fraction(2,3)) + Val(Fraction(1,3))
    assert(exp == Plus(Val(Fraction(2,3)), Val(Fraction(1,3))))
  }

  test("Simplify writing an expression") {
    val exp = Val(2,3) + Val(1,3)
    assert(exp == Plus(Val(Fraction(2,3)), Val(Fraction(1,3))))
    assert(ExpParser.eval(exp).get == Fraction(1,1))
  }

  test("Eval e3") {
    assert(ExpParser.eval(e3).get == Fraction(1, 104))
  }
}
