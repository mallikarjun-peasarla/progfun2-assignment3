package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    int <- Gen.chooseNum(Integer.MIN_VALUE, Integer.MAX_VALUE)
    heap <- oneOf[H](genHeap, empty)
  } yield {
    insert(int, heap)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a:Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (if (a > b) b else a)
  }

  property("emptyHeap") = forAll { a: Int =>
    val h1: H = insert(a, empty)
    deleteMin(h1) == empty  //
  }

  property("priority") = forAll { (h: H) =>
    delMinTillEmpty(h) == delMinTillEmpty(h).sorted
  }

  property("melding") = forAll { (h1: H, h2: H) =>
    val result = meld(h1, h1)
    val mergedMin = findMin(result)
    mergedMin == findMin(h1) || mergedMin == findMin(h2)
  }

  property("melding is associative") = forAll { (h:H, i:H, j:H) =>
    val a = meld(meld(h, i), j)
    val b = meld(h, meld(i, j))
    delMinTillEmpty(a) == delMinTillEmpty(b)
  }

  def delMinTillEmpty(h: H): List[Int] = {
    if(isEmpty(h)) {
      Nil
    } else {
      val min = findMin(h)
      min :: delMinTillEmpty(deleteMin(h))
    }
  }

}
