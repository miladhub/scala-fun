package greeter

object List {
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
  def List = new Nil
  def List[T](e1: T) = new Cons(e1, new Nil)
  def List[T](e1: T, e2: T) = new Cons(e1, new Cons(e2, new Nil))
  def List[T](e1: T, e2: T, e3: T) = new Cons(e1, new Cons(e2, new Cons(e3, new Nil)))
}
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  override def toString = if (isEmpty) "" else head + " " + tail  
}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}
class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.head")
}
object Main extends App {
  val l1 = List.List()
  val l2 = List.List(1)
  val l3 = List.List(1, 2)
  val l4 = List.List(1, 2, 3)
  
  println(l1)
  println(l2)
  println(l3)
  println(l4)
}