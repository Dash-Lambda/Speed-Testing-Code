package general_testing

import ichi.bench._
import spire.implicits._
import spire.math.SafeLong
import GenTester.benchRun
import spire.tailrec

object Runner {
  def main(args: Array[String]): Unit = {
    val inputs = Vector.range(50000, 101000, 1000)
    val id = 11
    val nam = "Primes"
    
    val funcs = Vector[(String, Int => Any)](
      ("Lazy", pLaz),
      ("Imperative", pImp),
      ("Imperative_Alt", pImpAlt),
      ("List_Recursion", pLstRec),
      ("List_Tail_Recursion", pLstTrec),
      ("Vector_Recursion", pVecRec),
      ("Vector_Tail_Recursion", pVecTrec)
    )
    
    benchRun(inputs, id, nam)(funcs)
  }
  
  //Lazy evaluation
  def pLaz(max: Int): List[Int] = {
    def odds: Stream[Int] = Stream.from(3, 2).takeWhile(n => n*n <= max)
    def composites: Stream[Int] = odds.flatMap(n => Stream.from(n*n, 2*n).takeWhile(_ <= max))
    2 +: Stream.from(3, 2).takeWhile(_ <= max).diff(composites).toList
  }
  
  //Imperative
  def pImp(max: Int): List[Int] = {
    val arr = Array.fill((max + 1)/2)(true)
    
    val bound = Math.sqrt(max).toInt
    cfor(3)(_ <= bound, _ + 2){i =>
      cfor(i*i)(_ <= max, _ + 2*i){composite =>
        arr(composite/2) = false
      }
    }
    
    2 +: (for(i <- arr.indices if arr(i)) yield 2*i + 1).tail.toList
  }
  
  //Imperative without only-odds optimization
  def pImpAlt(max: Int): List[Int] = {
    val arr = Array.fill(max + 1)(true)
    arr(0) = false
    arr(1) = false
    
    val bound = Math.sqrt(max).toInt
    cfor(2)(_ <= bound, _ + 1){i =>
      cfor(i*i)(_ <= max, _ + i){composite =>
        arr(composite) = false
      }
    }
    
    (for(i <- arr.indices if arr(i)) yield i).toList
  }
  
  //List Recursion
  def pLstRec(max: Int): List[Int] = {
    def pHRec(src: List[Int]): List[Int] = src match{
      case p +: cs => p +: pHRec(cs.filter(_%p != 0))
      case _ => List[Int]()
    }
    2 +: pHRec(List.range(3, max, 2))
  }
  
  //List Tail Recursion
  def pLstTrec(max: Int): List[Int] = {
    @tailrec
    def pHTrec(primes: List[Int], src: List[Int]): List[Int] = src match{
      case p +: cs => pHTrec(p +: primes, cs.filter(_%p != 0))
      case _ => primes.reverse
    }
    pHTrec(List[Int](2), List.range(3, max, 2))
  }
  
  //Vector Recursion
  def pVecRec(max: Int): Vector[Int] = {
    def pHRec(src: Vector[Int]): Vector[Int] = src match{
      case p +: cs => p +: pHRec(cs.filter(_%p != 0))
      case _ => Vector[Int]()
    }
    2 +: pHRec(Vector.range(3, max, 2))
  }
  
  //Vector Tail Recursion
  def pVecTrec(max: Int): Vector[Int] = {
    @tailrec
    def pHTrec(primes: Vector[Int], src: Vector[Int]): Vector[Int] = src match{
      case p +: cs => pHTrec(primes :+ p, cs.filter(_%p != 0))
      case _ => primes
    }
    pHTrec(Vector[Int](2), Vector.range(3, max, 2))
  }
}
