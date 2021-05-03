package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[A]
      h <- genHeap
    } yield meld(insert(a, empty), h),
    for {
      a <- arbitrary[A]
      b <- arbitrary[A]
      h <- genHeap
    } yield meld(insert(b, insert(a, empty)), h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("insert_min_and_find_min") = forAll { h: H =>
    val min = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(min, h)) equals min
  }

  property("insert_and_delete_in_empty_heap") = forAll { a: A =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("delete_and_reinsert_min") = forAll { h: H =>
    val heap = if (isEmpty(h)) insert(0, h) else h
    val min = findMin(heap)
    val heapWithoutMin = deleteMin(heap)
    findMin(insert(min, heapWithoutMin)) equals min
  }

  property("min_of_mins_is_min") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) || !isEmpty(h2)) ==> {
      val meldMin = findMin(meld(h1, h2))
      val h1Min = if (!isEmpty(h1)) findMin(h1) else findMin(h2)
      val h2Min = if (!isEmpty(h2)) findMin(h2) else h1Min
      meldMin equals ord.min(h1Min, h2Min)
    }
  }

  property("min_of_two_insertions_in_empty_heap") = forAll { (x: A, y: A) =>
    findMin(insert(y, insert(x, empty))) equals ord.min(x, y)
  }

  private def getElementsSeq(h: H): Seq[A] = {

    @tailrec
    def iterate(acc: Seq[A], remainingHeap: H): Seq[A] = {
      if (isEmpty(remainingHeap)) acc else iterate(acc :+ findMin(remainingHeap), deleteMin(remainingHeap))
    }

    iterate(Seq.empty, h)
  }

  property("meld_must_be_sorted") = forAll { elems: Seq[A] =>
    val heap = elems.foldLeft(empty)((ts, a) => insert(a, ts))
    getElementsSeq(heap) equals elems.sorted
  }
}