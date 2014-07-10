package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1 setSignal false
    in2 setSignal false
    run
    assert(out.getSignal === false, "or 1")
    
    in1 setSignal true
    run
    assert(out.getSignal === true, "or 2")
    
    in2 setSignal true
    run
    assert(out.getSignal === true, "or 3")
    
    in1 setSignal false
    run
    assert(out.getSignal === true, "or 4")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1 setSignal false
    in2 setSignal false
    run
    assert(out.getSignal === false, "or 1")
    
    in1 setSignal true
    run
    assert(out.getSignal === true, "or 2")
    
    in2 setSignal true
    run
    assert(out.getSignal === true, "or 3")
    
    in1 setSignal false
    run
    assert(out.getSignal === true, "or 4")
  }
  
  test("12demux test") {
    val in = new Wire
    val c0 = new Wire
    val o0, o1 = new Wire
    val control = List(c0)
    val output = List(o0, o1)
    demux(in, control, output)
    in setSignal true
    c0 setSignal false
    run
    assert(o0.getSignal === false,  "12demux o0")
    assert(o1.getSignal === true, "12demux 01")

    in setSignal true
    c0 setSignal true
    run
    assert(o0.getSignal === true,  "12demux o0")
    assert(o1.getSignal === false, "12demux 01")
    
    in setSignal false
    c0 setSignal true
    run
    assert(o0.getSignal === false,  "12demux o0")
    assert(o1.getSignal === false, "12demux 01")
  }

  test("14demux test") {
    val in = new Wire
    val c0,c1 = new Wire
    val o0, o1, o2, o3 = new Wire
    val control = List(c0, c1)
    val output = List(o0, o1, o2, o3)
    demux(in, control, output)
    in setSignal true
    c0 setSignal false
    c1 setSignal false
    run
    assert(o0.getSignal === false,  "14demux o0")
    assert(o1.getSignal === false, "14demux 01")
    assert(o2.getSignal === false, "14demux o2")
    assert(o3.getSignal === true, "14demux 03")
    
    in setSignal true
    c0 setSignal false
    c1 setSignal true
    run
    assert(o0.getSignal === false,  "14demux o0")
    assert(o1.getSignal === false, "14demux 01")
    assert(o2.getSignal === true, "14demux o2")
    assert(o3.getSignal === false, "14demux 03")
    
    in setSignal true
    c0 setSignal true
    c1 setSignal false
    run
    assert(o0.getSignal === false,  "14demux o0")
    assert(o1.getSignal === true, "14demux 01")
    assert(o2.getSignal === false, "14demux o2")
    assert(o3.getSignal === false, "14demux 03")
    
    in setSignal true
    c0 setSignal true
    c1 setSignal true
    run
    assert(o0.getSignal === true,  "14demux o0")
    assert(o1.getSignal === false, "14demux 01")
    assert(o2.getSignal === false, "14demux o2")
    assert(o3.getSignal === false, "14demux 03")
  }
  
  test("15demux test") {
    val in = new Wire
    val c0,c1 = new Wire
    val o0, o1, o2, o3, o5 = new Wire
    val control = List(c0, c1)
    val output = List(o0, o1, o2, o3)
    demux(in, control, output)
  }

}
