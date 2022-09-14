package euler.archives.page.one

import scala.annotation.tailrec

/**
 * Project working on Euler
 * New File created by ani in  Euler @ 10/09/2022  13:28
 */
object Helper {
  /**
   * Accessory Functions
   */
  @tailrec
  def isPrime(num: Long, pos: Long = 2): Boolean = {
    if (pos >= scala.math.sqrt(num) + 1) true
    else if (num % pos == 0) false
    else isPrime(num, pos + 1)
  }

  /**
   * WE want to programmatically generate  a 2d array
   * @return
   */
  def generateArrayOf20by20Int: Array[Array[Int]] = {
    import scala.util.Random
    val grid = Array.ofDim[Int](20,20)
    val r = Random
    for{row <- grid.indices
        column <- grid.indices} { grid(row)(column) = r.nextInt(100)}
    grid
  }

  def alphabeticValue(word: String) : Int = {
    val input = word.toLowerCase().replace("\"",""); //note the to lower case in order to treat a and A the same way
    var sum =0
    for( index <- input.indices ) {
      var pos :Int = (input.charAt(index) - 'a') + 1;
      sum += pos
    }
    sum

  }

  def singleRotation(startIndex: Int,numberToRotate: Int): Int = {
    var numbers = Seq[String]()
    val numberInString = numberToRotate.toString
    for( number <- startIndex until (numberInString.length) if startIndex >=0){
      numbers = numbers :+ numberInString.charAt(number).toString
    }
    for(number <- 0 until startIndex if number >= 0){
      numbers = numbers :+ numberInString.charAt(number).toString
    }
    if( numbers.nonEmpty)
      numbers.reduce{
        (acc, ele)=> acc + ele
      }.toInt else numberToRotate
  }

  @tailrec
  def rotateAndCheckIsPrime(number: Int, rotationCount: Int =0, numberToRotate:Int):Boolean = {
    if(!isPrime(number))
      return false
    if(number.toString.length == 1) return true
    val count = rotationCount + 1

    val nextNumber = singleRotation(count,numberToRotate)
    if(count == number.toString.length){
      return isPrime(nextNumber)
    }
    rotateAndCheckIsPrime(nextNumber, count,numberToRotate)
  }
}
