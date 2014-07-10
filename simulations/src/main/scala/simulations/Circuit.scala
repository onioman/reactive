package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig)}
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val na1, na2, na1na2And = new Wire
    inverter(a1, na1)
    inverter(a2, na2)
    andGate(na1, na2, na1na2And)
    inverter(na1na2And, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => 
        out match {
          case Nil =>
            throw new Exception("Wrong number of outputs")
          case o::List() => 
            def demuxAction() {
            	o.setSignal(in.getSignal)
            }
            in addAction demuxAction
          case os =>
            throw new Exception("Wrong number of outputs")
        }
      case c::cs =>
        val nc, nc_in, c_in = new Wire
        val half = out.length / 2
        inverter(c,nc)
        andGate(nc, in, nc_in)
        andGate(c, in, c_in)
        demux(nc_in, cs, out.drop(half))
        demux(c_in, cs, out.take(half))
    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }
  
  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("oin1", in1)
    probe("oin2", in2)
    probe("oout", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
    
    in1.setSignal(false)
    run
    
    in2.setSignal(false)
    run
  }
  
  def or2GateExample {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("o2in1", in1)
    probe("o2in2", in2)
    probe("o2out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
    
    in1.setSignal(false)
    run
    
    in2.setSignal(false)
    run
  }

  def demuxGateExample {
    val in = new Wire
    val c0 = new Wire
    val o0, o1 = new Wire
    val control = List(c0)
    val output = List(o0, o1)
    demux(in, control, output)
    probe("demux_in", in)
    probe("demux_c0", c0)
    probe("demux_o0", o0)
    probe("demux_o1", o1)
    in.setSignal(true)
    c0.setSignal(false)
    run
    
    c0.setSignal(true)
    run
    
    in.setSignal(false)
    run
  }
  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  //Circuit.andGateExample
  //Circuit.orGateExample
  //Circuit.or2GateExample
  Circuit.demuxGateExample
}
