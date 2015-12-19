package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("times hello") {
	  assert(times(string2Chars("hello")) === List(('h', 1), ('e', 1), ('l', 2), ('o', 1)))
    assert(times(string2Chars("lol")) === List(('l', 2), ('o', 1)))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("singleton") {
    new TestTrees {
    	assert(singleton(List(t1)) === true)
    	assert(singleton(List(t1, t2)) === false)
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine with move") {
    val leaflist = List(Leaf('e', 1), Leaf('f', 1), Leaf('g', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('g', 1), Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2), Leaf('t', 2), Leaf('x',4)))
  }

  test("create code tree") {
    val opt =
      Fork(
        Leaf('l',2),
        Fork(
            Leaf('o',1),
            Fork(
                Leaf('h',1),
                Leaf('e',1),List('h', 'e'),2),
            List('o', 'h', 'e'),3),
        List('l', 'o', 'h', 'e'),5)
    assert(createCodeTree("hello".toList) === opt)
  }
  
  test("decode some words") {
    new TestTrees {
      val t = createCodeTree("hello".toList)
      assert(decode(t, List()) === "".toList)
      assert(decode(t, List(0, 1, 0, 0)) === "lol".toList)
      assert(decode(t, List(1, 1, 0, 1, 1, 1, 1, 1, 1, 0)) === "heel".toList)
      assert(decode(t, List(1, 1, 1, 1, 1, 1, 0)) === "eel".toList)
    }
  }
  
  test("encode an empty string should be empty") {
    new TestTrees {
      assert(encode(t1)("".toList) === List())
    }
  }
  
  test("encode simple strings") {
    new TestTrees {
      assert(encode(t1)("a".toList) === List(0))
      assert(encode(t1)("b".toList) === List(1))
      assert(encode(t1)("ab".toList) === List(0, 1))
      assert(encode(t1)("ba".toList) === List(1, 0))
    }
  }
  
  test("encode hello strings") {
    new TestTrees {
      val t =
        Fork(
          Leaf('l',2),
          Fork(
              Leaf('o',1),
              Fork(
                  Leaf('h',1),
                  Leaf('e',1),List('h', 'e'),2),
              List('o', 'h', 'e'),3),
          List('l', 'o', 'h', 'e'),5)
      
      assert(encode(t)("l".toList) === List(0))
      assert(encode(t)("ll".toList) === List(0, 0))
      assert(encode(t)("o".toList) === List(1, 0))
      assert(encode(t)("e".toList) === List(1, 1, 1))
      assert(encode(t)("h".toList) === List(1, 1, 0))
      assert(encode(t)("lol".toList) === List(0, 1, 0, 0))
      assert(decode(t, encode(t)("helloheel".toList)) === "helloheel".toList)
    }
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("in case of an empty table, codeBits does nothing") {
    new TestTrees {
      intercept[NotImplementedError] {
        assert(codeBits(List())('x') === ???)
      }      
    }
  }
  
  test("given a table, codeBits reads it") {
    new TestTrees {
      val table = List(('h', List(1, 1, 0)), ('e', List(1, 1, 1)), ('l', List(0)), ('o', List(1, 0)))
      assert(codeBits(table)('h') === List(1, 1, 0))
      assert(codeBits(table)('e') === List(1, 1, 1))
      assert(codeBits(table)('l') === List(0))
      assert(codeBits(table)('o') === List(1, 0))
    }
  }
  
  test("given two tables, mergeCodeTables merges them") {
    new TestTrees {
      val t_1 = List(('h', List(1, 1, 0)), ('e', List(1, 1, 1)))
      val t_2 = List(('l', List(0)), ('o', List(1, 0)))
      val table = List(('h', List(1, 1, 0)), ('e', List(1, 1, 1)), ('l', List(0)), ('o', List(1, 0)))      
      assert(mergeCodeTables(t_1, t_2) === table)
      assert(mergeCodeTables(t_1, Nil) === t_1)
      assert(mergeCodeTables(Nil, t_1) === t_1)
      assert(mergeCodeTables(Nil, Nil) === Nil)
    }
  }
  
  test("given a tree, convert converts it into a table") {
    new TestTrees {
      val table = List(('l', List(0)), ('o', List(1, 0)), ('h', List(1, 1, 0)), ('e', List(1, 1, 1)))
      val tree =
        Fork(
          Leaf('l',2),
          Fork(
              Leaf('o',1),
              Fork(
                  Leaf('h',1),
                  Leaf('e',1),List('h', 'e'),2),
              List('o', 'h', 'e'),3),
          List('l', 'o', 'h', 'e'),5)
      assert(convert(tree) === table)
    }
  }
  
  test("quick-encode hello strings") {
    new TestTrees {
      val t =
        Fork(
          Leaf('l',2),
          Fork(
              Leaf('o',1),
              Fork(
                  Leaf('h',1),
                  Leaf('e',1),List('h', 'e'),2),
              List('o', 'h', 'e'),3),
          List('l', 'o', 'h', 'e'),5)
      
      assert(quickEncode(t)("l".toList) === List(0))
      assert(quickEncode(t)("ll".toList) === List(0, 0))
      assert(quickEncode(t)("o".toList) === List(1, 0))
      assert(quickEncode(t)("e".toList) === List(1, 1, 1))
      assert(quickEncode(t)("h".toList) === List(1, 1, 0))
      assert(quickEncode(t)("lol".toList) === List(0, 1, 0, 0))
      assert(decode(t, quickEncode(t)("helloheel".toList)) === "helloheel".toList)
    }
  }
  
  test("decode and quick-encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
