package Chapter3

import Chapter3.List.sum

object Chapter3Test {
  //3.1
  def main(arg: Array[String]): Unit = {
    var x = List(1,2,4,5) match {
      case Cons(x,Cons(2,Cons(4,_))) => x           // MatchError    这个匹配的 就说 函数 2，4元素的List
      case Nil => 42                                 // MatchError   这个匹配的是 空列表   如果是空列表下面的case 好像会报错
      case Cons(x,Cons(y,Cons(3,Cons(4,_)))) => x+y    //result  3
      case Cons(h,t)=>h+sum(t)            // result   15
      case _ => 101                           //result  101
    }
    println(x)
  }

}
