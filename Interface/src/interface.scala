
/**
  * Created by Nizar Mabroukeh on 3/2/17.
  */


object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {  // tail recursion
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def fact(n: Int): Int = {  // normal recursion
    if (n <= 1) 1
    else n * fact(n - 1)
  }

  def fibonacci(n: Int): Int = {  //fibonacci tail recursive
    @annotation.tailrec
    def go(i: Int, n: Int, fib_n_2: Int, fib_n_1: Int): Int =
      if (i > n) fib_n_1
      else go(i + 1, n, fib_n_1, fib_n_2 + fib_n_1)

    if (n == 0) 0
    else if (n <= 2) 1
    else go(3, n, 1, 1)
  }

  def fib(n: Int): Int = {  // Fibonacci normal recursion
    if (n <= 0) 0
    else if (n <= 2) 1
    else fib(n-1) + fib(n-2)
  }

  def ascendingOrder(a: Int, b: Int): Boolean =
    a < b

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int, y: Boolean): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1, true)
    loop(0, true)
  }

  /*
     The result is a higher-order function that takes a function of two arguments and partially applies it.
     That is, if we have an A and a function that needs both A and B to produce C, we can get a function that just
     needs B to produce C (since we already have the A)
   */
  def partial[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)
    // or
    //(b: B) => f(a, b)

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => partial(a, f(b: B,c: C))


  private def formatResult(name: String, x: Int, f: Int => Int) = {
    val msg = "The %s value of %d is %d"
    msg.format(name, x, f(x))
  }

  def main(args: Array[String]): Unit =
    println(formatResult("Absolute", -42, abs))
}