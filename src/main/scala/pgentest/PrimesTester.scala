package pgentest

import java.io.{File, PrintWriter}

import common.TryAll
import ichi.bench.Thyme.Benched
import ichi.bench._
import spire.implicits._
import spire.tailrec

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source
import scala.util.{Failure, Success}
import resource._

object PrimesTester {
  def main(args: Array[String]): Unit = {
    val min = 1000
    val max = 100000
    val stp = (n: Int) => n+1000
    
    val id = 4
    val sel = 1
    
    sel match{
      case 2 =>
        val check = List(pImp(max), pLaz(max), pLstRec(max), pLstTrec(max), pVecRec(max), pVecTrec(max))
        println(check.mkString("\n"))
      case 0 =>
        val oFile = new PrintWriter(new File("Bench_Lump_3_100-10000_NEW.txt"))
        managed(Source.fromFile("Bench_Lump_3_100-10000.txt")).acquireAndGet{rsc =>
          for(dats <- rsc.getLines.map(lst => lst.split(" "))){
            oFile.println(dats(1))
          }
        }
        oFile.close()
      case 1 =>
        val tst = Thyme.warmedBench(verbose = println)
        val datamap = mutable.HashMap[Int, List[Double]]()
        
        cfor(min)(_ <= max, stp){i =>
          val dats = benchAllTryDat(id, i, tst)
          datamap += ((i, dats))
          println
        }
        
        val oFile = new PrintWriter(new File(s"Bench_Lump_${id}_$min-$max.txt"))
        for(k <- datamap.keys.toList.sorted){
          oFile.println(s"$k ${datamap(k).map(_*1000).mkString(" ")}")
        }
        oFile.close()
    }
  }
  
  def benchAllTryDat(id: Int, max: Int, tst: Thyme): List[Double] = {
    def plz = pLaz(max)
    def pim = pImp(max)
    def plr = pLstRec(max)
    def plt = pLstTrec(max)
    def pvr = pVecRec(max)
    def pvt = pVecTrec(max)
    
    val logFile = new PrintWriter(new File(s"Bench_Log_${id}_$max.txt"))
    def writer(str: String): Unit = {
      println(str)
      logFile.println(str)
    }
    
    val dat = ArrayBuffer[Double]()
    
    writer(s"Benching\nID: $id\nMax: $max")
    
    benchSingle(tst, writer, dat)("Imperative...")(pim)
    benchSingle(tst, writer, dat)("List Tail Recursion...")(plt)
    benchSingle(tst, writer, dat)("Vector Tail Recursion...")(pvt)
    benchSingle(tst, writer, dat)("List Recursion...")(plr)
    benchSingle(tst, writer, dat)("Vector Recursion...")(pvr)
    benchSingle(tst, writer, dat)("Lazy Stream...")(plz)
    
    logFile.close()
    
    val datFile = new PrintWriter(new File(s"Bench_Data_${id}_$max.txt"))
    datFile.print(dat.mkString(" "))
    datFile.close()
    
    dat.toList
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
  
  def pLaz(max: Int): List[Int] = {
    def odds: Stream[Int] = Stream.from(3, 2).takeWhile(n => n*n <= max)
    def composites: Stream[Int] = odds.flatMap(n => Stream.from(n*n, 2*n).takeWhile(_ <= max))
    2 +: Stream.from(3, 2).takeWhile(_ <= max).diff(composites).toList
  }
  
  def pImp(end: Int): List[Int] = {
    val arr = Array.fill((end + 1)/2)(true)
    
    val bound = Math.sqrt(end).toInt
    for (i <- 3 to end by 2 if i <= bound; composite <- i * i to end by 2 * i) {
      arr(composite/2) = false
    }
    
    2 +: (for (i <- arr.indices if arr(i)) yield 2 * i + 1).tail.toList
  }
  
  def pLstRec(max: Int): List[Int] = {
    def pHRec(lst: List[Int]): List[Int] = lst match{
      case p::ps => p +: pHRec(ps.filter(_%p != 0))
      case _ => List[Int]()
    }
    2 +: pHRec(List.range(3, max, 2))
  }
  
  def pLstTrec(max: Int): List[Int] = {
    @tailrec
    def pHTrec(primes: List[Int], src: List[Int]): List[Int] = src match{
      case p::ps => pHTrec(primes :+ p, ps.filter(_%p != 0))
      case _ => primes
    }
    pHTrec(List[Int](2), List.range(3, max, 2))
  }
  
  def pVecRec(max: Int): Vector[Int] = {
    def pHRec(vec: Vector[Int]): Vector[Int] = {
      if(vec.nonEmpty){
        val p = vec.head
        p +: pHRec(vec.tail.filter(_%p != 0))
      }else{
        Vector[Int]()
      }
    }
    2 +: pHRec(Vector.range(3, max, 2))
  }
  
  def pVecTrec(max: Int): Vector[Int] = {
    def pHTrec(primes: Vector[Int], src: Vector[Int]): Vector[Int] = {
      if(src.nonEmpty){
        val p = src.head
        pHTrec(primes :+ p, src.tail.filter(_%p != 0))
      }else{
        primes
      }
    }
    pHTrec(Vector[Int](2), Vector.range(3, max, 2))
  }
  
  
}