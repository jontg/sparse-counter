package net.ulfhedinn.util.counter

import scala.collection.mutable.HashMap

class DenseFrequencyCounter[A] extends FrequencyCounter[A] {
  var counter = new HashMap[A,Int]

  def increment(x: A) = {
    if(counter contains x) counter(x) += 1
    else counter += x -> 1
    counter(x)
  }

  def count(x: A) = {
    if(counter contains x) counter(x)
    else 0
  }

}