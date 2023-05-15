
## Pre-Reqs

Scala CLI:

```bash
# with Coursier
cs install scala-cli
```

## Usage


### IDE Setup (Metals)

```bash
scala-cli setup-ide . -v -S 2.13
```

### REPL

```bash
scala-cli repl -S 2.13

# OR
scala-cli repl -S 2.13 --dep org.scalacheck::scalacheck:1.17.0 .
```

### Demo

parsing & evaluating Lisp:

```scala
// ➤ scala-cli repl -S 2.13 --dep org.scalacheck::scalacheck:1.17.0  .
// Compiling project (Scala 2.13.10, JVM)
// [warn] ./lisp.scala:74:82
// [warn] match may not be exhaustive.
// [warn] It would fail on the following input: Uqt(_)
// [warn]   def eval(e: Sexpr, env: Map[String, Value] = defaultEnv): Either[Err, Value] = e match {
// [warn]                                                                                  ^
// [warn] Exhaustivity analysis reached max recursion depth, not all missing cases are reported.
// [warn] (Please try with scalac -Ypatmat-exhaust-depth 40 or -Ypatmat-exhaust-depth off.)
// Compiled project (Scala 2.13.10, JVM)
// Welcome to Scala 2.13.10 (OpenJDK 64-Bit Server VM, Java 17.0.6).
// Type in expressions for evaluation. Or try :help.

// scala> import tut._
import tut._

//scala>
    lispy.eval("""
     | (let ((mk-adder (lambda (x) (lambda (y) (+ y x))))
     |         (add3 (mk-adder 3)))
     |     (if (>= (add3 1) 1) 1 2))
     |       
     |   """
     | )
val res1: Either[tut.pc.Err,tut.lispy.Value] = Right(Lit(1))
```

Simple calculator:

```scala
//scala> 
    calc.expr(
     |  """
     |  7 * 7 + (8 / 3) - 33
     |  """
     | )
val res6: Either[tut.pc.Err,(Int, Seq[Char])] = Right((18,))
```

### Cleanup

Or, to just start over:
```bash
rm -rf .bsp .scala-build .metals

# find and kill suspect bsp/bloop/metals processes
jps -lmv
```
