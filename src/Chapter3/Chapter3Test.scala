package Chapter3

import Chapter3.List.{append2Left, append2Right, drop, dropWhile, dropWhileCurry, dropWhileMyShelf, foldRight, init, length, length2, length3, product2, setHead, sum, sum3, tail}

import scala.Byte.MaxValue.<

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
//    var headList = setHead(List(1, 2, 3, 4, 5),9)
//    println(headList)
//    headList = setHead(Nil,9)
//    println(headList)


    //3.4
    var dropList = drop(List(1),2)
    println("dropList: "+dropList)

    //3.5myshelf
    val list:List[Int] = List(1, 2, 3, 4, 5, 3, 5, 3)
    val dropWhiles = dropWhileMyShelf(list, (x: Int) => x==3)
    println("dropWhiles: " + dropWhiles)

    //3.5
    val dropWhileList = dropWhile(list,(x:Int)=>x<3)
    println("dropWhileList: "+dropWhileList)

    //3.6
    val initList = init(List(1,2,3,4,5,6))
    println("initList: "+initList)

    //3.3.2
    val xs:List[Int] = List(1,2,3,4,5)
    val ex1 = dropWhileCurry(xs)(x=>x<4)
    println("ex1: "+ex1)


    //3.7
    // 看的答案是不会，然后我自己验证发现开始入参为0.0计算的时间会比 后面为0.0计算的时间要多  这一点有点可以证明答案的正确性 但是不知道是不是有其他的因素影响
    val l1:List[Double] = List(0.0,1.1,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3)
    val l2:List[Double] = List(1.1,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3)
    val s1 = System.currentTimeMillis()
    val ex_l1 = product2(l1)
    val s2 = System.currentTimeMillis()-s1
    val s3 = System.currentTimeMillis()
    val ex_l2 = product2(l2)
    val value1 = foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _))
    val value2 = foldRight(List(1, 2, 3, 4), List(5,6,7,8))(Cons(_, _))
    println(value1)
    println(value2)

    //3.9
    //1+length2(Cons(2, 3, Nil))
    //1+1+length2(Cons(3,Nil))
    //1+1+1+length2(Nil)
    //1+1+1+0
    val i = length(List(1, 2, 3))
    println("3.9 List长度为:"+ i);

    //3.10
    //foldLeft(List(2,3,4),f(0+1))(f)
    //foldLeft(List(3,4),f(1+2))(f)
    //foldLeft(List(4),f(3+3))(f)
    //foldLeft(List(),f(6+4))(f)
    val i1 = sum3(List(1, 2, 3,4))
    println("3.10 List的和为:"+ i1);

    //3.11
    val i3 = length3(List(1, 2, 3,4,5,6))
    println("3.11 List长度为:" + i3);

    //3.14
    val value3 = List(1, 2, 3)
    val value4 = List(4,5,6)
    val reslutRight = append2Right(value3,value4)
    val reslutLeft = append2Left(value3,value4)
    println("reslutRight为:" + reslutRight);
    println("reslutLeft为:" + reslutLeft);


  }


  }
