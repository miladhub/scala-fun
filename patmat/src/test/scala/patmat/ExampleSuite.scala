package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Example._

@RunWith(classOf[JUnitRunner])
class ExampleSuite extends FunSuite {

  test("weight of a larger tree") {
    assert(removeAt(0, List(1,2,3,4)) === List(2,3,4))
    assert(removeAt(1, List(1,2,3,4)) === List(1,3,4))
    assert(removeAt(2, List(1,2,3,4)) === List(1,2,4))
    assert(removeAt(3, List(1,2,3,4)) === List(1,2,3))
  }
}
