package mpbustamante

class PracticeAlgorithms {

  /** Find the last element of a list **/
  def last[A](as: List[A]) : A = {
    if (as.isEmpty) throw new Exception ("Something")
    else if (as.length == 1) as.head
    else last(as.tail)
  }

  def penultimate[A](as: List[A]) : A = {
    if (as.isEmpty || as.length == 1) throw new Exception ("Something")
    else if (as.length == 2) as.head
    else last(as.tail)
  }

  def nth[A](n: Int, as: List[A]) : A = {
    if (as.isEmpty) throw new Exception ("Something")
    else if (n == 1) as.head
    else nth(n-1,as.tail)
  }

  def length[A](as: List[A]) : Int = {
    if (as.isEmpty) 0
    else 1 + length(as.tail)
  }

  def reverse[A](as: List[A]) : List[A] = {
    if (as.isEmpty) as
    else reverse(as.tail) ++ List(as.head)
  }

  def isPalindrome[A](as: List[A]) : Boolean = {
    if (as.isEmpty || as.length == 1) true
    else {
      if (as.head != reverse(as).head) false
      else isPalindrome(reverse(as.tail).tail)
    }
  }

  def flatten[A](as: List[List[A]]) : List[A] = {
    if (as.isEmpty) Nil
    else as.head ++ flatten(as.tail)
  }

  def compress[A](as: List[A]) : List[A] = {
    if (as.isEmpty || as.length == 1) as
    else {
      if (as.head == as.tail.head) compress(as.tail)
      else List(as.head) ++ compress(as.tail)
    }
  }

  def pack[A](as: List[A]) : List[List[A]] = {
    def packAux(headList: List[A], as: List[A]) : List[List[A]] = {
      if (headList.head == as.head) packAux(headList ++ List(as.head), as.tail)
      else List(headList)
    }

    if (as.isEmpty) Nil
    else if (as.length == 1) List(as)
    else {
      packAux(List(as.head),as.tail) ++ pack(as.tail)
    }
  }
}

object PracticeAlgorithms {
  def main(args: Array[String]): Unit = {
    val pa = new PracticeAlgorithms
    val myList = List(1,1,1,3,3,2,2)
    //println(pa.last(myList))

    println(pa.pack(myList))

  }
}
