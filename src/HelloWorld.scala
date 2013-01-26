import net.ulfhedinn.util.counter._
import java.io._

import scala.util.MurmurHash

object HelloWorld {

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("data/Bible.txt")
    val lines = source.mkString.replaceAll("[^a-zA-Z ]", " ").toLowerCase
    source.close()

    var counter = new DenseFrequencyCounter[String]
    var counter2 = new SparseFrequencyCounter[String](320, 1)
    var counter3 = new HybridFrequencyCounter[String](5, 320, 1)

    lines.split(" +").map(l => {
      val s = l
      counter increment s
      counter2 increment s
      counter3 increment s
    })

    println("Dense\tSparse\tHybrid\tString\t" + lines.split(" +").length)

    counter3.top.reverse.take(20).map(l => {
      val s = l._1
      println((counter count s) + "\t" + (counter2 count s) + "\t" + (counter3 count s) + "\t" + s)
    })

    def f = new java.io.File("Results")
    val text = "Dense\tSparse\tHybrid\tString\t" + lines.split(" +").length + "\n" + counter.counter.toSeq.sortBy(_._2).reverse.map(l => {
      val s = l._1
      (counter count s) + "\t" + (counter2 count s) + "\t" + (counter3 count s) + "\t" + s
    }).mkString("\n")

    val fw = new FileWriter("test.txt"); fw.write(text); fw.close()
  }

}
