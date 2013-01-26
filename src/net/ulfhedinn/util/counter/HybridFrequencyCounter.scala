package net.ulfhedinn.util.counter

class HybridFrequencyCounter[A](numTopList: Int, numBuckets: Int, hashSeed: Int) extends FrequencyCounter[A] {
  var sorter = (a: (A, Int), b: (A, Int)) => a._2 < b._2
  var top: List[(A, Int)] = Nil
  private var sparse: SparseFrequencyCounter[A] = new SparseFrequencyCounter[A](numBuckets, hashSeed)

  def increment(x: A) = {
    val count = sparse increment x
    top.filter(_._1 == x) match {
      case Nil => {
        if (top.length < numTopList) top = ((x, count) :: top).sort(sorter)
        else if (top.first._2 < count) top = ((x, count) :: top.tail).sort(sorter)
        count
      }
      case t => {
        top = (x, t.head._2 + 1) :: (top remove (_._1 == x)).sort(sorter)
        t.head._2
      }
    }
  }

  def count(x: A) = {
    top.filter(_._1 == x) match {
      case Nil => sparse count x
      case t => t.head._2
    }
  }
}