package Chapter2

object MyMoudle {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }


  private def formatAbs(x: Int) = {
    val msg = "the Absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int) = {
    val msg = "the factorial value of %d is %d"
    msg.format(x, factorial(x))
  }

  private def formatResult(name: String, x: Int, f: Int => Int) = {
    val msg = "the %s of %d is %d"
    msg.format(name, x, f(x))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)

  }

  def main(arg: Array[String]): Unit = {
    println(formatResult("Absolute value", 5, abs))
    println(formatResult("factorial value", 5, factorial))
  }

}
