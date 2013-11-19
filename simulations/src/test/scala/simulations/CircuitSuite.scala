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
    in1.signal = false
    in2.signal = false
    run

    assert(out.signal === false, "and 1")

    in1.signal = true
    run

    assert(out.signal === false, "and 2")

    in2.signal = true
    run

    assert(out.signal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.signal = false
    in2.signal = false
    run

    assert(out.signal === false, "or 1")

    in1.signal = true
    run

    assert(out.signal === true, "or 2")

    in2.signal = true
    run

    assert(out.signal === true, "or 3")

    in1.signal = false
    run

    assert(out.signal === true, "or 4")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.signal = false
    in2.signal = false
    run

    assert(out.signal === false, "or2 1")

    in1.signal = true
    run

    assert(out.signal === true, "or2 2")

    in2.signal = true
    run

    assert(out.signal === true, "or2 3")

    in1.signal = false
    run

    assert(out.signal === true, "or2 4")
  }

  test("demux") {
    val in = new Wire
    val s = List.empty[Wire]
    val d = List(new Wire)

    demux(in, s, d)
    run

    assert(d === List(new Wire))

    in.signal = true
    run

    val expected = new Wire
    expected.signal = true

    assert(d === List(expected))
  }
}
