package general_testing

import java.io.{File, PrintWriter}

import common.TryAll
import ichi.bench.Thyme
import ichi.bench.Thyme.Benched

import scala.collection.mutable.ArrayBuffer
import spire.implicits._

import scala.util.{Failure, Success}

object GenTester extends App{
  def benchRun[T](inputs: Vector[T], id: Int, title: String)(funcs: Vector[(String, T => Any)]): Unit = {
    val tst = Thyme.warmedBench(verbose = println)
    
    //Open up log files
    val logFile = new PrintWriter(new File(s"Bench_${title}_${id}_Log_${inputs.head}-${inputs.last}.txt"))
    val datFile = new PrintWriter(new File(s"Bench_${title}_${id}_Lump_${inputs.head}-${inputs.last}.txt"))
    
    //Make the header
    datFile.println(s"Inputs ${funcs.map(_._1).mkString(" ")}")
    
    //Bench the functions for each input
    for(inp <- inputs){
      benchAllLog(title, id, inp, logFile, datFile, tst)(funcs)
    }
    
    //Close log files
    logFile.close()
    datFile.close()
  }
  
  //Take a single input and bench all functions at that input
  def benchAllLog[T](title: String, id: Int, inp: T, logFile: PrintWriter, dataLog: PrintWriter, tst: Thyme)(funcs: Vector[(String, T => Any)]): Unit = {
    def logWriter(str: String): Unit = {
      println(str)
      logFile.println(str)
    }
    
    logWriter(s"Benching: $title\nID: $id\nInput: $inp")
    
    val dat = ArrayBuffer[Double]()
    for((name, func) <- funcs){
      logWriter(s"\n--$name...")
      dat += benchSingle(logWriter, tst)(func(inp))
    }
    
    logWriter("")
    logFile.flush()
    
    dataLog.println(s"$inp ${dat.mkString(" ")}")
    dataLog.flush()
  }
  
  //Bench a single function at a single input, catch any errors, and return runtime
  def benchSingle(logWriter: String => Unit, tst: Thyme)(func: => Any): Double = {
    val bres = Benched.empty
    
    TryAll(tst.benchWarm(tst.Warm(func))(bres)) match{
      case Success(_) =>
        logWriter(bres.toString)
        1000*bres.runtime
      case Failure(e) =>
        logWriter(s"Failed: $e")
        -1.0
    }
  }
}
