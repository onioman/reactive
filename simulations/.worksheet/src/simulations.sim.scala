package simulations

import math.random

object sim extends EpidemySimulator {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(122); 
  println("Welcome to the Scala worksheet");$skip(26); 
  
  val ids = 0 until 10;System.out.println("""ids  : scala.collection.immutable.Range = """ + $show(ids ));$skip(60); 
  val population = List(ids.map((x: Int) => new Person(x)));System.out.println("""population  : List[scala.collection.immutable.IndexedSeq[simulations.sim.Person]] = """ + $show(population ));$skip(23); 
  
  val rand = random;System.out.println("""rand  : Double = """ + $show(rand ));$skip(23); val res$0 = 
  
  List.range(1, 10);System.out.println("""res0: List[Int] = """ + $show(res$0))}
  
}
