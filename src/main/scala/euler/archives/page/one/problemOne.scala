package euler.archives.page.one

import euler.archives.page.one.Helper._

import scala.annotation.tailrec

/**
 * Project working on Euler
 * New File created by ani in  Euler @ 02/09/2022  16:43
 */
object problemOne {

  final val grid = Array(
    Array(8,  2, 22, 97, 38, 15,  0, 40,  0, 75,  4,  5,  7, 78, 52, 12, 50, 77, 91,  8),
    Array(49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48,  4, 56, 62,  0),
    Array(81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30,  3, 49, 13, 36, 65),
    Array(52, 70, 95, 23,  4, 60, 11, 42, 69, 24, 68, 56,  1, 32, 56, 71, 37,  2, 36, 91),
    Array(22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80),
    Array(24, 47, 32, 60, 99,  3, 45,  2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50),
    Array(32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70),
    Array(67, 26, 20, 68,  2, 62, 12, 20, 95, 63, 94, 39, 63,  8, 40, 91, 66, 49, 94, 21),
    Array(24, 55, 58,  5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72),
    Array(21, 36, 23,  9, 75,  0, 76, 44, 20, 45, 35, 14,  0, 61, 33, 97, 34, 31, 33, 95),
    Array(78, 17, 53, 28, 22, 75, 31, 67, 15, 94,  3, 80,  4, 62, 16, 14,  9, 53, 56, 92),
    Array(16, 39,  5, 42, 96, 35, 31, 47, 55, 58, 88, 24,  0, 17, 54, 24, 36, 29, 85, 57),
    Array(86, 56,  0, 48, 35, 71, 89,  7,  5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58),
    Array(19, 80, 81, 68,  5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77,  4, 89, 55, 40),
    Array(4, 52,  8, 83, 97, 35, 99, 16,  7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66),
    Array(88, 36, 68, 87, 57, 62, 20, 72,  3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69),
    Array(4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18,  8, 46, 29, 32, 40, 62, 76, 36),
    Array(20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74,  4, 36, 16),
    Array(20, 73, 35, 29, 78, 31, 90,  1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57,  5, 54),
    Array(1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52,  1, 89, 19, 67, 48),
  )

  /**
   * Multiples of 3 or 5
   * https://projecteuler.net/problem=1
   * @param num
   * @return
   */
  def multiplesOf3And5(num : Int=1000): Int = {
    var sum =0;
    for(nm <- 1 until (num) if (nm % 3)==0 || (nm % 5)==0){
        sum += nm
    }
    sum
  }

  /**
   * Even Fibonacci numbers
   * https://projecteuler.net/problem=2
   * @param limit
   * @return
   */
  def evenFibonacciNumbers(limit: Int = 4_000_000) :Int = {
    require(limit>2, "Method requirement failed")
    var evenSum = 2
    var data = Vector(1,2)
    var i = 2
    for(numb <-  3 to(limit)  if (numb == data(i-1) + data(i-2)) ){
      if(numb % 2 ==0){
        evenSum = numb + evenSum
      }
      data = data :+ numb
      i +=1
    }
    evenSum
  }

//  todo: refactor the above code to make use of recursion







  /**
   * Problem 7
   * What is the 10 001st prime number?
   * https://projecteuler.net/problem=7
   */
  @tailrec
  def findPrimeNumberAtPosition(curPos: Long = 1, primeCount: Long = 0): Long = {



    val count = primeCount + (if (isPrime(curPos)) 1 else 0)

    if (count == 10001) curPos
    else findPrimeNumberAtPosition(curPos + 1, count)

  }




  def largestProductInGrid(grid: Array[Array[Int]]=problemOne.grid) : Int = {
    var (c1,c2,c3,c4,r1,r2,r3,r4, max) = (0,0,0,0,0,0,0,0,Integer.MIN_VALUE)
    var check = 0;
    for{row <- grid.indices
        column <- grid.indices} {
      // while on the current column we want to go forward by three row+

      if(row +3 < grid.length){
//        check the product of the grid column and row
        check = grid(column)(row)* grid(column)(row+1)*grid(column)(row+2)*grid(column)(row+3)
        if(check>max){
          max = check
//          saving the indices for use later
          r1 = row
          r2 = row+1
          r3 = row + 2
          r4 = row + 3
          c1 = column
          c2 = column
          c3 = column
          c4 = column
        }
      }
//      while on the current row we want to go three column forward
      if(column + 3 < grid.length){
//        check the product of the grid column and rows
        check = grid(column)(row) * grid(column+1)(row)* grid(column+2)(row) * grid(column+3)(row)
        if(check > max){
            max = check
          //          saving the indices for use later
          r1 = row
          r2 = row
          r3 = row
          r4 = row
          c1 = column
          c2 = column +1
          c3 = column + 2
          c4 = column + 3
        }
      }

//      we want to go diagonally for row and column, meaning as we move one row, we would move one column, this would produce a diagonal like structure
      if((row +3  < grid.length) && (column +3 < grid.length)){
        //        check the product along the diagonal downward

        check = grid(column)(row) * grid(column+1)(row+1)* grid(column+2)(row+2) * grid(column+3)(row+3)
        if(check > max){
          max = check
          r1 = row
          r2 = row + 1
          r3 = row + 2
          r4 = row + 3
          c1 = column
          c2 = column +1
          c3 = column + 2
          c4 = column + 3
        }
      }
    // we want to go diagonally for row and column, meaning as we move one row,
    // we would move one column, this would produce a diagonal like structure but this time around upward
      if(row > 3 && (column +3 < grid.length)){
        //        check the product along the diagonal upward
        check =  grid(column)(row) * grid(column +1)(row -1) * grid(column+2)(row-2) * grid(column +3)(row-3)
        if(check > max ){
          max = check
          r1 = row
          r2 = row - 1
          r3 = row - 2
          r4 = row - 3
          c1 = column
          c2 = column +1
          c3 = column + 2
          c4 = column + 3
        }
      }

    }


    max
  }

  def totalNameScores:Int = {
    import scala.io.Source
    val bufferedSource = Source.fromFile("p022_names.txt")
    var text: String = ""
    for (line <- bufferedSource.getLines()) {
      text += line
    }
    bufferedSource.close

    val names = text.split(",").toSeq
    val sum = names.filter(_.nonEmpty).map(_.trim.toLowerCase()).sorted.zipWithIndex
      .map{
        case (str,i) =>
          val index = i + 1
          alphabeticValue(str) * index
      }.sum
    sum

  }


  @tailrec
  def rotateCircularlyRecursively(number: Int = 1_000_000, circularPrimeCount: Int=0):Int = {
    val count = circularPrimeCount + (if (rotateAndCheckIsPrime(number,numberToRotate = number)) + 1 else 0 )
    val nextNumber = number - 1
    if(nextNumber == 2){
     return count + 1
    }
    rotateCircularlyRecursively(nextNumber,count)
  }



}



