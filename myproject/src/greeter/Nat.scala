package greeter

abstract class Nat {
  val actual: Int
  override def toString = actual.toString()
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat =
    if (that.isZero) this
    else successor + that.predecessor
  def - (that: Nat): Nat =
    if (that.isZero) this
    else predecessor - that.predecessor
}

object Zero extends Nat {
  val actual = 0
  def isZero: Boolean = true
  def predecessor: Nat = throw new RuntimeException
}

class Succ(val n: Nat) extends Nat {
  val actual = n.actual + 1
  def isZero: Boolean = false
  def predecessor: Nat = n
}

object Nats extends App {
  val n0 = Zero
  val n1 = new Succ(Zero)
  val n3 = new Succ(new Succ(n1))
  val n2 = n3 - n1
  val n0again = n3 - n3
  
  val n5 = new Succ(new Succ(new Succ(new Succ(n1))))
  val n3again = n5 - n2
  
  println(n0)
  println(n0again)
  println(n1)
  println(n2)
  println(n3)
  println(n5)
  println(n3again)
}