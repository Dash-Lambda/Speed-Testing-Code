import java.io.{File, PrintWriter}

import scala.io.Source
import resource._

object Scratch {
  def main(args: Array[String]): Unit = {
    val dats = managed(Source.fromFile("Bench_Primes_11_Lump_1000-50000.txt")).acquireAndGet{rsc =>
      rsc.getLines.toList.tail.map{str =>
        val arr = str.split(" ")
        (arr.head, arr.tail.map(n => 1000*n.toDouble))
      }
    }
    
    val oFile = new PrintWriter(new File("NewData.txt"))
    for((num, dat) <- dats){
      oFile.println(s"$num ${dat.mkString(" ")}")
    }
    oFile.close()
  }
}
