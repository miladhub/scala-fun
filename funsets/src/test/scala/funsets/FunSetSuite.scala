package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect contains the elements of the first set which are also contained in the second set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      
      assert(contains(intersect(s12, s1), 1), "1 is equal to 1, 2 intersect 1")
      assert(contains(intersect(s12, s2), 2), "2 is equal to 1, 2 intersect 2")
      assert(contains(intersect(s23, s3), 3), "3 is equal to 2, 3 intersect 3")
      assert(!contains(intersect(s23, s3), 2), "2 does not belong to the intersection of 2, 3 and 3")
    }
  }
  
  test("diff contains the elements of the first set which are not contained in the second set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      
      assert(contains(diff(s12, s23), 1), "1, 2 without 2, 3 contains 1")
      assert(!contains(diff(s12, s23), 2), "1, 2 without 2, 3 does not contain 2")
      assert(!contains(diff(s12, s23), 3), "1, 2 without 2, 3 does not contain 3")
    }
  }
  
  test("filter selects the elements of the set that satisfy the predicate") {
	  def even(x: Int): Boolean = x % 2 == 0
    def odd(x: Int): Boolean = x % 2 == 1
    
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s123 = union(s12, s23)
      
      assert(contains(filter(s123, even), 2), "2 is the only even number in 1, 2, 3")
      assert(!contains(filter(s123, even), 1), "1 is not an even number in 1, 2, 3")
      assert(!contains(filter(s123, even), 3), "3 is not an even number in 1, 2, 3")
      assert(!contains(filter(s123, even), 42), "42 is not an even number in 1, 2, 3")
      
      assert(!contains(filter(s123, odd), 2), "2 is not an odd number in 1, 2, 3")
      assert(contains(filter(s123, odd), 1), "1 is an odd number in 1, 2, 3")
      assert(contains(filter(s123, odd), 3), "3 is an odd number in 1, 2, 3")
      assert(!contains(filter(s123, odd), 43), "43 is not an odd number in 1, 2, 3")
    }
  }
  
  test("forall tells whether a predicate holds true for all the elements of a given set") {
    def even(x: Int): Boolean = x % 2 == 0
    def odd(x: Int): Boolean = x % 2 == 1
    val s4 = singletonSet(4)
    
    new TestSets {
      val s12 = union(s1, s2)
      val s13 = union(s1, s3)
      val s23 = union(s2, s3)
      val s123 = union(s12, s23)
      val s24 = union(s2, s4)
      
      assert(forall(s123, x => even(x) || odd(x)), "1, 2, 3 are even or odd")
      assert(!forall(s123, even), "1, 2, 3 are not all even")
      assert(!forall(s123, odd), "1, 2, 3 are not all odd")
      assert(forall(s13, odd), "1, 3 are odd")
      assert(forall(s24, even), "2, 4 are even")
    }
  }
  
  test("exists tells whether a predicate holds true for at least an element of a given set") {
    def even(x: Int): Boolean = x % 2 == 0
    def odd(x: Int): Boolean = x % 2 == 1
    val s4 = singletonSet(4)
    
    new TestSets {
      val s12 = union(s1, s2)
      val s13 = union(s1, s3)
      val s23 = union(s2, s3)
      val s123 = union(s12, s23)
      val s24 = union(s2, s4)
      
      assert(exists(s123, x => even(x) || odd(x)), "1, 2, 3 contains at least an even or odd number")
      assert(exists(s123, even), "1, 2, 3 contains an even number")
      assert(exists(s123, odd), "1, 2, 3 contains an odd number")
      assert(!exists(s13, even), "1, 3 does not contain even numbers")
      assert(!exists(s24, odd), "2, 4 does not contain odd numbers")
      assert(exists(s24, even), "2, 4 contains even numbers")
      assert(!exists(s24, x => x == 42), "2, 4 does not contain 42")
    }
  }
  
  test("map creates a new set by applying a function to each element of the given set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s13 = union(s1, s3)
      val s23 = union(s2, s3)
      val s123 = union(s12, s23)
      
      def square(x: Int): Int = x * x
      
      assert(contains(map(s123, square), 1), "1, 4, 9 contains 1")
      assert(contains(map(s123, square), 4), "1, 4, 9 contains 4")
      assert(contains(map(s123, square), 9), "1, 4, 9 contains 9")
      assert(!contains(map(s123, square), 3), "1, 4, 9 does not contain 3")
      assert(!contains(map(s123, square), 10), "1, 4, 9 does not contain 10")
      
      def identity(x: Int): Int = x
      
      assert(contains(map(s123, identity), 1), "1, 2, 3 contains 1")
      assert(contains(map(s123, identity), 2), "1, 2, 3 contains 2")
      assert(contains(map(s123, identity), 3), "1, 2, 3 contains 3")
      assert(!contains(map(s123, identity), 4), "1, 2, 3 does not contain 4")
      assert(!contains(map(s123, identity), 10), "1, 2, 3 does not contain 10")
    }
  }
}
