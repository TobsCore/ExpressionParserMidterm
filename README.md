# Expression Parser and Evaluator

This project started is my implementation of the programming paradigms midterm.

## How to run it

This program can be run by following the steps below:

```
> git clone https://github.com/TobsCore/ExpressionParserMidterm
> cd ExpressionParserMidterm
> sbt console
```

This will compile the project and launch the sbt console, which then can be used to write and evaluate expressions as follows

```
scala> val e5 = Multiply(Val(Fraction(10,13)), Val(Fraction(12,14)))
```

`e5` encodes the expression `(10/13) * (12/14)`

This expression parser supports some simplification for writing the above expression as follows

```
scala> val e5_short = Val(10,13) * Val(12,14)
```

## Pretty printing and Evaluating terms

In order to present the expressions in a readable format, the expression parser `ExpParser` can be used to pretty-print any expression. This can be done in the scala console:

```
scala> ExpParser.pretty(e5)
res0: String = ((10/13)*(12/14))
```

Evaluation is done by finding the greatest common divisor and by applying the mathematical rules to the terms.

```
scala> ExpParser.eval(e5)
res3: Option[Fraction] = Some(Fraction(60,91))
```

This is achieved, by calling the `simplify()` method on the functions, which can be called from anywhere.

```
scala> val exp = Fraction(2,4)
exp: Fraction = Fraction(2,4)

scala> exp.simplify()
res4: Option[Fraction] = Some(Fraction(1,2))
```
