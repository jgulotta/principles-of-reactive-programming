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

  test("demux 0") {
    val in = new Wire
    val s = List.empty[Wire]
    val d = List(new Wire)
    val e = new Wire
    val expected = List(e)

    demux(in, s, d)
    run

    assert(d === expected)

    in.signal = true
    run

    e.signal = true

    assert(d === expected)
  }

  test("demux 1") {
    val in = new Wire
    val s = List(new Wire)
    val d = List(new Wire, new Wire)
    val e0, e1 = new Wire
    val expected = List(e0, e1)

    demux(in, s, d)
    run

    assert(d === expected)

    in.signal = true
    run

    e0.signal = true

    assert(d === expected)

    s(0).signal = true
    in.signal = false
    run

    e0.signal = false
    assert(d === expected)

    in.signal = true
    run

    e1.signal = true
    assert(d === expected)
  }

  test("demux 2") {
    val in = new Wire
    val s = List(new Wire, new Wire)
    val d = List(new Wire, new Wire, new Wire, new Wire)
    val e0, e1, e2, e3 = new Wire
    val expected = List(e0, e1, e2, e3)

    demux(in, s, d)
    run

    assert(d === expected)

    in.signal = true
    run

    e0.signal = true

    assert(d === expected)

    s(0).signal = true
    in.signal = false
    run

    e0.signal = false
    assert(d === expected)

    in.signal = true
    run

    e1.signal = true
    assert(d === expected)

    s(0).signal = false
    s(1).signal = true
    in.signal = false
    run

    e1.signal = false
    assert(d === expected)

    in.signal = true
    run

    e2.signal = true
    assert(d === expected)

    s(0).signal = true
    in.signal = false
    run

    e2.signal = false
    assert(d === expected)

    in.signal = true
    run

    e3.signal = true
    assert(d === expected)
  }
}
