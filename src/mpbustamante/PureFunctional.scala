package mpbustamante

import scala.annotation.tailrec

/**
  * This is my singleton
  */
object PureFunctional {

  /**
    * This method cannot be called from any code outside my PureFuncional object
    * As it's private, it doesn't need to explicitly define the output type, although
    * it's considered a good practice in methods that can be called from outside
    * It doesn't contain curly braces (block) as it's only one code line
    * @param x the x
    * @return a string like "Hello world! x"
    */
  private def myPureFunction(x: Int) = "Hello world! " + x

  /**
    * This method uses string interpolation for printing the value of z
    * @param z the z
    */
  def printResult(z: Any) : Unit = println(s"$z")

  /**
    * This is a sample function to apply with foldLeft
    * @param a first string
    * @param b second string
    * @return the largest of them by length
    */
  def largestName(a: String, b: String) : String = if (a.length > b.length) a else b

  /**
    * This is an example of a function that accepts a function as parameter
    * It's called a higher-order function
    * @param functionName the function name
    * @param number the number
    * @param f the function
    * @return a string that contains the information of the result
    */
  def formatResult(functionName: String, number: Int, f: Int => Int) : String = {
    val msg = "The result of my function %s is %d."
    msg.format(functionName,f(number))
  }

  /**
    * This is an example of recursive factorial with tailrec annotation inside
    * @param n the number
    * @return the factorial
    */
  def factorial(n: Int): Int = {
    @tailrec def go(n: Int, acc: Int): Int = {
      if (n < 0) acc
      else go(n-1, n*acc)
    }
    go(n,1)
  }

  /**
    * This is an example of recursive fibonacci, simple
    * @param pos the position
    * @return the fibonacci
    */
  private def fib(pos: Int): Int = {
    if (pos == 1)
      0
    else if (pos == 2)
      1
    else
      fib(pos - 1) + fib(pos - 2)
  }

  /**
    * This is an example of tailrec fibonacci
    * @param pos the position
    * @return the fibonacci
    */
  private def fibTailRec (pos: Int): Int = {
    @tailrec def fibTailRecHelper (pos: Int, prev: Int = 0, next: Int = 1) : Int = pos match {
      case 0 => prev
      case 1 => next
      case _ => fibTailRecHelper (pos-1, next, next + prev)
    }
    fibTailRecHelper(pos)
  }

  def upward(a: Int, b: Int) : Boolean = a < b

  /**
    * Exercise 2.2
    * This is a polymorphic function that has a tailrec inside
    * @param as the array
    * @param ordered the ordering function
    * @tparam A the type
    * @return
    */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    @tailrec def isSortedHelper(elem: A, as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
      if (!as.isEmpty) {
        if (as.length > 2)
          ordered(elem, as.head) && isSortedHelper(as.head, as.tail, ordered)
        else
          ordered(elem, as.head)
      }
      else false
    }

    if (!as.isEmpty)
      isSortedHelper(as.head, as.tail, ordered)
    else
      false

  }

  val functionAsValue = new Function2[Int, Int, Boolean] {
    def apply(a: Int, b: Int) = a < b
  }

  /** MAIN **/
  def main(args: Array[String]): Unit = {
    printResult(myPureFunction(1))

    val names = List("Susana","Esther","Almudena")
    printResult(names.reduceLeft(largestName))

    println(formatResult("fibTailRec", 5,fibTailRec))

    //Anonymous function absurd sample
    val numbers = List(1,4,3,5,6,7)
    printResult(numbers.reduce(_*_+1))

    printResult(isSorted(Array(1,2,3,4,5), upward))

    //This is a call to apply for this function defined as value
    functionAsValue.apply(1,2)
  }
}