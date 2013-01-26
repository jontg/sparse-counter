package net.ulfhedinn.util.counter

import scala.util.MurmurHash
import scala.util.Random

class SparseFrequencyCounter[A](numBuckets: Int, hashSeed: Int) extends FrequencyCounter[A] {
  private var buckets: Array[Int] = new Array[Int](numBuckets)
  private var hashes: Array[MurmurHash[A]] = new Array[MurmurHash[A]]((numBuckets + 31) / 32)

  for (i <- 0 until numBuckets) buckets(i) = 0
  for (i <- 0 until (numBuckets + 31) / 32) hashes(i) = new MurmurHash[A](i + hashSeed)

  private def getHashStrings(x: A): Array[Int] = {
    hashes.flatMap(h => {
      h.reset()
      h.apply(x)
      val s = h.hash.toBinaryString
      ("0" * (32 - s.length) + s).map(c => c match {
        case '0' => -1
        case '1' => 1
      })
    })
  }

  private def computeValue(l: Array[Int]): Int =
    l.sum / l.length

  def increment(x: A): Int = {
    var hashedVals = getHashStrings(x)
    buckets = (buckets zip hashedVals).map(hb => hb._1 + hb._2)
    computeValue((buckets zip hashedVals).map(hb => hb._1 * hb._2))
  }

  def count(x: A): Int = {
    var hashedVals = getHashStrings(x)
    computeValue((buckets zip hashedVals).map(hb => hb._1 * hb._2))
  }

}