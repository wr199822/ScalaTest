package Chapter3


/**
 *
 * @author wangrui
 * @date 2024/1/10
 * @Description
 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List {
  def sum(ints: List[Int]):Int= ints match {
    case Nil => 0
    case Cons(x,xs) => x+ sum(xs)
  }

  def product(ds:List[Double]):Double = ds match{
    case Nil => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def  apply[A](as:A*):List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head,apply(as.tail:_*))
  }

  //3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil // 空列表返回空列表
    case Cons(_, tailList) => tailList
  }
  //3.3
  def setHead[A](l: List[A], h: A): List[A] = l match{
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }


  //3.4
  def drop[A](list: List[A],n:Int):List[A] = {
    if(n<=0) {
      list
    }else{
      list match{
        case Nil => Nil
        case Cons(_,t)=>drop(t,n-1)
      }
    }
  }

  //3.5
  def dropWhileMyShelf[A](l:List[A],f:A=>Boolean):List[A] =  l match{
    case Nil => Nil
    case Cons(head,t) if(f(head))=>  dropWhileMyShelf(t,f)
    case Cons(head,t) if(!f(head))=> Cons(head,dropWhileMyShelf(t,f))
  }

  //3.5 书上的答案
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match{
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }


  def append[A](a1:List[A],a2:List[A]):List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h,append(t,a2))
  }

  //3.6
  def initMyshelf[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,initMyshelf(t));
  }
  //3.6 书上答案
  def init[A](l: List[A]): List[A] =
    l match{
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil)  => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  //这里的逻辑就是当遇到最后一个元素的时候把前面的元素都复制
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
//      case Cons(_, Nil) => List(buf.toList*)   不知道导入那个
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }


  def dropWhileCurry[A](l:List[A])(f:A=> Boolean):List[A]= {
    //match 的case 后面居然还能带if
    l match {
      case Cons(h,t) if f(h) => dropWhileCurry(t)(f)
      case _=> l
    }
  }


  def foldRight[A,B](as:List[A],z:B)(f:(A,B)=>B):B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x,foldRight(xs,z)(f))
    }

  def sum2(ns:List[Int]) =
    foldRight(ns,0)((x,y)=>x+y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_*_)

  //3.7
  //3.8
  //3.9
  def length[A](as:List[A]):Int=
    foldRight(as,0)((_,acc)=>acc+1)
  def length2[A](as:List[A]):Int= as match {
    case Nil => 0
    case Cons(_,t)=>length2(t)+1
  }

  //3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
    }

  //3.11
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((x,_)=>x+1)

  //3.12
  def reverse[A](as:List[A]):List[A] =
    foldLeft(as, List[A]())((acc:List[A],h) => Cons(h,acc))


  def reverse2[A](l: List[A]): List[A] = {
    def reverseHelper(original: List[A], reversed: List[A]): List[A] = original match {
      case Nil => reversed
      case Cons(h, t) => reverseHelper(t, Cons(h, reversed))
    }

    reverseHelper(l, Nil)
  }

  //3.13  不会

  //3.14
  def append2Right[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1,a2)(Cons(_,_))

  def append2Left[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1),a2)((x:List[A],y:A)=>Cons(y,x))

  //3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  //有点绕 有机会在想
//  def concatLeft[A](l: List[List[A]]): List[A] =
//    foldLeft(reverse(l), Nil: List[A])(append2Left)
  def concat2[A](l: List[List[A]]): List[A] = l match {
    case Nil => List()
    case Cons(head,tail)=> append(head,concat2(tail))
  }
}
