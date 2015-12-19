package patmat

/**
 * @author daneel
 */
object Example {
  def removeAt[T](n: Int, xs: List[T]): List[T] =
    if (n == 0)
      xs.tail
    else
      xs.head :: removeAt(n - 1, xs.tail)
}