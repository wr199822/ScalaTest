package Chapter3

import Chapter3.List.{drop, dropWhile, init, setHead, sum, tail}

object Chapter3Test {
  //3.1
  def main1(arg: Array[String]): Unit = {
    var x = List(1,2,4,5) match {
      case Cons(x,Cons(2,Cons(4,_))) => x           // MatchError    这个匹配的 就说 函数 2，4元素的List
      case Nil => 42                                 // MatchError   这个匹配的是 空列表   如果是空列表下面的case 好像会报错
      case Cons(x,Cons(y,Cons(3,Cons(4,_)))) => x+y    //result  3
      case Cons(h,t)=>h+sum(t)            // result   15
      case _ => 101                           //result  101
    }
    println(x)


  }


  def main(arg: Array[String]): Unit = {
    //3.2
    val value = tail(List(1, 2, 3, 4, 5))
    println(value)

    //3.3
    var headList = setHead(9,List(1, 2, 3, 4, 5))
    println(headList)
    headList = setHead(9,Nil)
    println(headList)


    //3.4
    var dropList = drop(List(1),2)
    println("dropList: "+dropList)

    //3.5
    val list = List(1, 2, 3, 4, 5,3,5,3)
    val dropWhileList = dropWhile(list,(x:Int)=>x==3)
    println("dropWhileList: "+dropWhileList)

    //3.6
    val initList = init(List(1,2,3,4,5,6))
    println("initList: "+initList)


  }


  }
