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
    //该注解是确保这个函数是一个尾递归
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)

  }

  def isSorted[A](as: Array[A], ordered: (A,A)=> Boolean): Boolean ={
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n==as.length) true
      else if (ordered(as(n-1),as(n))) loop(n + 1)
      else false
    }

    if (as.length < 1) throw new Exception("Array length cannot lt 1")
    else loop(1)
  }




  //partiall
  def partiall[A,B,C](a:A,f:(A,B)=>C):B=>C =
    (b:B) => f(a,b)

  //2.3
  def curry[A,B,C](f:(A,B)=>C) : A=>(B=>C) =
    (a:A) =>(b:B)=> f(a,b)

  //2.4
  def uncurry[A,B,C](f:A =>B =>C):(A,B) =>C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f:B=>C,g:A=>B):A=>C =
    (a:A)=>f(g(a))

  def main(arg: Array[String]): Unit = {
    println( isSorted(Array(3, 2, 1),(x:Int,y:Int)=> x-y>0))
  }

}
