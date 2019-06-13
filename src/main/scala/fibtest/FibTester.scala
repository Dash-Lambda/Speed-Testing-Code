package fibtest

import java.io.{File, PrintWriter}

import common.TryAll
import ichi.bench.Thyme
import ichi.bench.Thyme.Benched
import spire.implicits._
import spire.math.{Integral, SafeLong}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}



object FibTester {
  def main(args: Array[String]): Unit = {
    val ranges = Vector[(Int, Int, Int => Int)]((10000, 1000000, _ + 10000))
    val id = 1
    val nam = "Fibonacci"
    
    for(r <- ranges){
      benchRun(r._1, r._2, r._3, id, nam)
    }
  }
  
  def benchRun(min: Int, max: Int, stp: Int => Int, id: Int, nam: String): Unit = {
    val tst = Thyme.warmedBench(verbose = println)
    val logFile = new PrintWriter(new File(s"Bench_${nam}_${id}_Log_$min-$max.txt"))
    val datFile = new PrintWriter(new File(s"Bench_${nam}_${id}_Lump_$min-$max.txt"))
  
    cfor(min)(_ <= max, stp){i =>
      val dats = benchAllTryDat(nam, id, i, logFile, tst)
      datFile.println(s"$i ${dats.mkString(" ")}")
      datFile.flush()
      println
    }
  
    logFile.close()
    datFile.close()
  }
  
  def benchAllTryDat(nam: String, id: Int, max: Int, logFile: PrintWriter, tst: Thyme): Vector[Double] = {
    def fim = fibNumFast(max)
    def faz = fibItr(max)
    
    def writer(str: String): Unit = {
      println(str)
      logFile.println(str)
    }
    
    val dat = ArrayBuffer[Double]()
    
    writer(s"Benching\nName: $nam\nID: $id\nMax: $max")
    
    benchSingle(tst, writer, dat)("Imperative...")(fim)
    benchSingle(tst, writer, dat)("Lazy...")(faz)
    
    writer("")
    
    logFile.flush()
    
    dat.toVector
  }
  
  def benchSingle[A](tst: Thyme, writer: String => Unit, dat: ArrayBuffer[Double])(title: String)(func: => A): Unit = {
    val bres = Benched.empty
    
    writer(s"\n$title")
    TryAll(tst.benchWarm(tst.Warm(func))(bres)) match{
      case Success(_) =>
        writer(bres.toString)
        dat += bres.runtime
      case Failure(_) =>
        writer("Failed")
        dat += -1.0
    }
  }
  
  def fibItr: Stream[SafeLong] = Stream.iterate((SafeLong(0), SafeLong(1))){case (a: SafeLong, b: SafeLong) => (b, a + b)}.map(_._1)
  
  def fibNumFast(num: SafeLong): SafeLong = {
    var a = SafeLong(0)
    var b = SafeLong(1)
    var c = SafeLong(1)
    
    cfor(SafeLong(0))(_ < num, _ + 1){_ =>
      a = b
      b = c
      c = a + b
    }
    
    a
  }
}
