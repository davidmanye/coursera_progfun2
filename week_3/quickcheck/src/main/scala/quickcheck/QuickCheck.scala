package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._
import scala.math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val min = if (a < b) a else b
    val m = insert(a, insert(b, empty))
    findMin(m) == min
  }

  property("insert and delete min") = forAll { (a: Int) =>
    val inserted = insert(a, empty)
    val deleted = deleteMin(inserted)
    deleted == empty
  }

  property("del2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(deleteMin(h)) == max(a, b)
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val min1 = if (isEmpty(h1)) 0 else findMin(h1)
    val min2 = if (isEmpty(h2)) 0 else findMin(h2)
    val min = if (min1 < min2) min1 else min2
    min == (if (isEmpty(h)) 0 else findMin(h))
  }

  property("meld2") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(meld(h, empty)))
  }

  property("delMin3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(deleteMin(h))) == scala.math.max(c, scala.math.max(a, b))
  }

  property("ordered finding min and deleting") = forAll { h: H =>
    def ordered(heap: H): List[Int] = {
      if (isEmpty(h)) List()
      else {
        val min = findMin(h)
        min :: ordered(deleteMin(h))
      }
    }

    def isOrdered(list: List[Int]): Boolean = list match {
      case List() => true
      case x :: List() => true
      case x :: xs if x < xs.head => isOrdered(xs)
      case _ => false
    }

    isOrdered(ordered(h))
  }

}
