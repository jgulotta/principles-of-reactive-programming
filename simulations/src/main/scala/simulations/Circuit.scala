package simulations

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def signal: Boolean = sigVal
  def getSignal = signal
  
  def signal_=(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }
  def setSignal(s: Boolean) = signal = s

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }

  override def toString = sigVal.toString
  override def equals(o: Any): Boolean = {
    o match {
      case w: Wire => w.sigVal == sigVal
      case _ => false
    }
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
          "  " + currentTime + ": " + name + " -> " + wire.signal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.signal
      afterDelay(InverterDelay) {
        output.signal = !inputSig
      }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.signal
      val a2Sig = a2.signal
      afterDelay(AndGateDelay) {
        output.signal = a1Sig & a2Sig
      }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.signal
      val a2Sig = a2.signal
      afterDelay(OrGateDelay) {
        output.signal = a1Sig | a2Sig
      }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1n, a2n, a1n_a2n = new Wire
    inverter(a1, a1n)
    inverter(a2, a2n)
    andGate(a1n, a2n, a1n_a2n)
    inverter(a1n_a2n, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def rec_demux(s: List[Wire], d: List[Wire]) {
      if (s.isEmpty) d(0).signal = in.signal
      else if (s.size == 1) {
        val w = s(0)
        val wn = new Wire
        inverter(w, wn)
        andGate(in, wn, d(0))
        andGate(in, w, d(1))
      } else rec_demux(s.tail, d.dropRight(2))
    }

    rec_demux(c, out)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample() {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.signal = false
    in2.signal = false
    run

    in1.signal = true
    run

    in2.signal = true
    run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample()
}
