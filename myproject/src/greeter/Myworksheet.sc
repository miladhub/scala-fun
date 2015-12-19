package greeter
 
object intsets {
	var t1 = new NonEmpty(3, Empty, Empty)    //> t1  : greeter.NonEmpty = {.3.}
	var t2 = new NonEmpty(4, Empty, Empty)    //> t2  : greeter.NonEmpty = {.4.}
	var t3 = t2 union t1                      //> t3  : greeter.IntSet = {.3{.4.}}
	var l1 = List.singleton(1)                //> l1  : greeter.Cons[Int] = 1 
	var l2 = new Cons(2, l1)                  //> l2  : greeter.Cons[Int] = 2 1 
	
	def nth[T](n: Int, l: List[T]): T =
    if (l.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) l.head
    else nth(n - 1, l.tail)                       //> nth: [T](n: Int, l: greeter.List[T])T
  
	nth(1, l2)                                //> res0: Int = 1
}
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
	def union(other: IntSet): IntSet =
		((left union right) union other) incl elem
	override def toString = "{" + left + elem + right + "}"
}
object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
  override def toString = "."
}