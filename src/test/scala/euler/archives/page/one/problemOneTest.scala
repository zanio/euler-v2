package euler.archives.page.one

import euler.archives.page.one.Helper._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Project working on Euler
 * New File created by ani in  Euler @ 02/09/2022  16:44
 */
class problemOneTest extends AnyFlatSpec with Matchers{
  "Problem One -> Method multiplesOf3And5" should "return the sum of all the mutiples within that number" in {
    problemOne.multiplesOf3And5() shouldEqual 233168
  }
  "Problem One -> Method multiplesOf3And5" should "return the wrong if the sum of all the mutiples within that number" in {
    problemOne.multiplesOf3And5() should  equal(233168)
  }

  "Problem One -> Method evenFibonacciNumbers" should "return the actual result when 4_000_000 is limit is passed" in {
    problemOne.evenFibonacciNumbers() should  equal(4613732)
  }

  "Problem One -> Method evenFibonacciNumbers" should "handle -1 parameter" in {
    an[IllegalArgumentException] should be thrownBy(problemOne.evenFibonacciNumbers(-1))

  }

  "Problem One -> Method evenFibonacciNumbers" should "handle 0 parameter" in {
    an[IllegalArgumentException] should be thrownBy(problemOne.evenFibonacciNumbers(0))

  }
  "Problem One -> Method isPrime" should "return the true" in {
    isPrime(5) should  equal(true)
  }
  "Problem One -> Method findPrimeNumberAtPosition" should "return the position of the prime number" in {
    problemOne.findPrimeNumberAtPosition() should  equal(104743)
  }

  "Problem One -> Method generateArrayOf20by20Int" should "return a 2d array" in {
    val grid = generateArrayOf20by20Int
    grid(0)(0) should not be equal(102)
  }

  "Problem One -> Method largestProductInGrid" should "return a 2d array" in {
    problemOne.largestProductInGrid() should equal(70600674)
  }

  "Problem One -> Method totalNameScores" should "check that the answer is correct" in {
    problemOne.totalNameScores should equal(871198282)
  }

  "Problem One -> Method rotateAndCheckIsPrime" should "check that the answer is correct" in {
    rotateAndCheckIsPrime(7,numberToRotate = 7) should equal(true)
  }

  "Problem One -> Method rotateCircularlyRecursively" should "check that the answer is correct when below 100" in {
    problemOne.rotateCircularlyRecursively(100) should equal(13)
  }
 "Problem One -> Method rotateCircularlyRecursively" should "check that the answer is correct when below 1_000_000" in {
    problemOne.rotateCircularlyRecursively() should equal(55)
  }

  "Problem One -> Method singleRotation" should "check that the answer is correct" in {
   singleRotation(1,3601) should equal(6013)
   singleRotation(2,3651) should equal(5136)
   singleRotation(-1,3651) should equal(3651)
  }

}
