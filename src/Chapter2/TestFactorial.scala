package Chapter2

object TestFactorial{

        def fib(x:Int):Int={
        def go(x:Int):Int=
        if(x<=2)x-1
        else go(x-1)+go(x-2)

        go(x);
        }

        def main(arg:Array[String]):Unit={
        println(fib(7));
        }
        }
