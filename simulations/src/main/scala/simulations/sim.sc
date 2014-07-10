package simulations

import math.random

object sim extends EpidemySimulator {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val ids = 0 until 10                            //> ids  : scala.collection.immutable.Range = Range(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                  //| )
  val population = List(ids.map((x: Int) => new Person(x)))
                                                  //> population  : List[scala.collection.immutable.IndexedSeq[simulations.sim.Per
                                                  //| son]] = List(Vector(simulations.EpidemySimulator$Person@6910fe28, simulation
                                                  //| s.EpidemySimulator$Person@5a9e29fb, simulations.EpidemySimulator$Person@45d6
                                                  //| 4c37, simulations.EpidemySimulator$Person@7825d2b2, simulations.EpidemySimul
                                                  //| ator$Person@7b2be1bd, simulations.EpidemySimulator$Person@7df17e77, simulati
                                                  //| ons.EpidemySimulator$Person@79a5f739, simulations.EpidemySimulator$Person@7f
                                                  //| 09fd93, simulations.EpidemySimulator$Person@68e6ff0d, simulations.EpidemySim
                                                  //| ulator$Person@1572e449))
  
  val rand = random                               //> rand  : Double = 0.28272852799756243
  
  List.range(1, 10)                               //> res0: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  
}