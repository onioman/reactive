package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("gen1") = forAll { h: H => 
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m,h)) == m
  }
  
  property("inserTwo") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val minAB = if (a < b) a else b
    findMin(h) == minAB
  }
  
  property("becomeEmpty") = forAll { a: Int =>
  	val h = insert(a, empty)
  	isEmpty(deleteMin(h))
  }
  
  property("increasingInsert") = forAll{ (a: Int, b: Int, c: Int) =>
    def insertFromList(l: List[Int], h: H) : H = l match {
      case Nil => h
      case x::xs => insertFromList(xs, insert(x,h))
    }
    def checkOrder(l: List[Int], h: H) : Boolean = l match {
      case Nil => true
      case x::xs => if (x > findMin(h)) false else checkOrder(xs, deleteMin(h))
    }
    val l = List(a, b, c)
    checkOrder(l.sorted, insertFromList(l.sorted, empty))
  }
  
  property("minOfMelding") = forAll{ (h1: H, h2: H) => 
  	val melded = meld(h1, h2)
  	val min = findMin(melded)
  	min == findMin(h1) || min == findMin(h2)
  }
 
  lazy val genHeap: Gen[H] = for {
		head <- arbitrary[Int]
		rest <- oneOf(empty, genHeap)
  } yield insert(head, rest)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
