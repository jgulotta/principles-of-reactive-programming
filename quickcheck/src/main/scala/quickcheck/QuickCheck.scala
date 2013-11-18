package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll {
    a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  def toList(hh: H): List[Int] = {
    if (isEmpty(hh)) return List.empty
    findMin(hh) :: toList(deleteMin(hh))
  }

  property("sorted") = forAll {
    h: H =>
      val l = toList(h)
      l.sorted == l
  }

  property("delete3") = forAll { (a: Int, b: Int, c: Int) =>
    val reversed = List(a, b, c).sorted.reverse
    val h = (reversed :\ empty)(insert)
    findMin(deleteMin(h)) == reversed(1)
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
