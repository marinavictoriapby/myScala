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

  /** MAIN **/
  def main(args: Array[String]): Unit = {
    printResult(myPureFunction(1))

    val names = List("Susana","Esther","Almudena")
    printResult(names.reduceLeft(largestName))

    println(formatResult("fibTailRec", 5,fibTailRec))

    //Anonymous function absurd sample
    val numbers = List(1,4,3,5,6,7)
    printResult(numbers.reduce(_*_+1))

  }
}