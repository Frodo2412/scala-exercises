package me.brunolemus.interview
package list

import scala.annotation.{tailrec, unused}


sealed abstract class RList[+T] {

  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  def ::[S >: T](elem: S) = new::(elem, this)

  // Easy Exercises
  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def --(index: Int): RList[T]

  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]

  // Medium Exercises
  def rle: RList[(T, Int)]

  def duplicateEach(times: Int): RList[T]

  def rotate(positions: Int): RList[T]

}

case object RNil extends RList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException("head of empty list")

  override def tail: RList[Nothing] = throw new NoSuchElementException("tail of empty list")

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = this

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def --(index: Int): RList[Nothing] = throw new NoSuchElementException

  override def map[S](f: Nothing => S): RList[S] = this

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = this

  override def filter(f: Nothing => Boolean): RList[Nothing] = this

  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(times: Int): RList[Nothing] = RNil

  override def rotate(positions: Int): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailRec(remainingList: RList[T], acc: String): String = remainingList match {
      case RNil => acc
      case head :: RNil => acc + head
      case head :: tail => toStringTailRec(tail, acc + s"$head, ")
    }

    "[" + toStringTailRec(this, "") + "]"
  }

  override def apply(index: Int): T = {

    @tailrec
    def applyTailRec(remainingList: RList[T], currentIndex: Int): T =
      if (currentIndex < 0) throw new IndexOutOfBoundsException
      else if (currentIndex == 0) remainingList.head else applyTailRec(remainingList.tail, currentIndex - 1)

    @tailrec
    @unused
    def applyTailRec_v2(remainingList: RList[T], currentIndex: Int): T =
      if (currentIndex == index) remainingList.head
      else applyTailRec_v2(remainingList.tail, index + 1)

    if (index < 0) throw new NoSuchElementException else applyTailRec(this, index)

  }

  override def length: Int = {
    @tailrec
    def lengthTailRec(remainingList: RList[T], currentLength: Int): Int = remainingList match {
      case RNil => currentLength
      case _ :: tail => lengthTailRec(tail, currentLength + 1)
    }

    lengthTailRec(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailRec(remainingList: RList[T], accumulator: RList[T]): RList[T] = remainingList match {
      case RNil => accumulator
      case ::(head, tail) => reverseTailRec(tail, head :: accumulator)
    }

    reverseTailRec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatTailRec(remainingItems: RList[S], accumulator: RList[S]): RList[S] = remainingItems match {
      case RNil => accumulator
      case head :: tail => concatTailRec(tail, head :: accumulator)
    }

    concatTailRec(anotherList, this.reverse).reverse
  }

  override def --(index: Int): RList[T] = {
    @tailrec
    def removeTailRec(remaining: RList[T], currentIndex: Int, accumulated: RList[T]): RList[T] = remaining match {
      case RNil => throw new NoSuchElementException
      case head :: tail =>
        if (currentIndex == index) accumulated.reverse ++ tail
        else removeTailRec(tail, currentIndex + 1, head :: accumulated)
    }

    removeTailRec(this, 0, RNil)
  }

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailRec(accum: RList[S], remaining: RList[T]): RList[S] =
      remaining match {
        case RNil => accum.reverse
        case head :: tail => mapTailRec(f(head) :: accum, tail)
      }

    mapTailRec(RNil, this)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapTailRec(accum: RList[S], remaining: RList[T]): RList[S] =
      remaining match {
        case RNil => accum.reverse
        case head :: tail => flatMapTailRec(f(head).reverse ++ accum, tail)
      }

    flatMapTailRec(RNil, this)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterTailRec(accum: RList[T], remaining: RList[T]): RList[T] =
      remaining match {
        case RNil => accum.reverse
        case head :: tail =>
          filterTailRec(
            if (f(head)) head :: accum else accum,
            tail
          )
      }

    filterTailRec(RNil, this)
  }

  override def rle: RList[(T, Int)] = {
    @tailrec
    @unused
    def tailRecRle(remaining: RList[T], accumulated: RList[(T, Int)]): RList[(T, Int)] = remaining match {
      case RNil => accumulated
      case head :: tail =>
        val current = remaining.filter(_ == head)
        val newTuple = (head, current.length)
        tailRecRle(tail.filter(_ != head), newTuple :: accumulated)
    }

    @tailrec
    def tailRecRle_v2(remaining: RList[T], currentTuple: (T, Int), accumulated: RList[(T, Int)]): RList[(T, Int)] = remaining match {
      case RNil if currentTuple._2 == 0 => accumulated
      case RNil => currentTuple :: accumulated
      case head :: _ if head == currentTuple._1 => tailRecRle_v2(remaining.tail, currentTuple.copy(_2 = currentTuple._2 + 1), accumulated)
      case head :: tail => tailRecRle_v2(tail, (head, 1), currentTuple :: accumulated)
    }

    tailRecRle_v2(this.tail, (this.head, 1), RNil).reverse

  }

  override def duplicateEach(times: Int): RList[T] = {
    @tailrec
    def duplicateEachTailRec(remaining: RList[T], duplications: Int, accumulator: RList[T]): RList[T] = remaining match {
      case RNil => accumulator
      case head :: tail if duplications > 0 => duplicateEachTailRec(head :: tail, duplications - 1, head :: accumulator)
      case _ :: tail => duplicateEachTailRec(tail, times, accumulator)
    }

    @tailrec
    @unused
    def duplicateTailRec(remaining: RList[T], currentElement: T, nDuplications: Int, accumulator: RList[T]): RList[T] =
      if (remaining.isEmpty && nDuplications == times) accumulator
      else if (remaining.isEmpty) duplicateTailRec(remaining, currentElement, nDuplications + 1, currentElement :: accumulator)
      else if (nDuplications == times) duplicateTailRec(remaining.tail, remaining.head, 0, accumulator)
      else duplicateTailRec(remaining, currentElement, nDuplications + 1, currentElement :: accumulator)

    duplicateEachTailRec(this, times, RNil).reverse
  }

  override def rotate(positions: Int): RList[T] = {
    @tailrec
    def rotateTailRec(remaining: RList[T], currentElement: T, moved: Int, accumulated: RList[T]) = remaining match {
      case RNil if moved == 0 => ???
      case ::(head, tail) => ???
    }
  }
}

object RList {

  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def fromTailRec(remaining: Iterable[T], accumulator: RList[T]): RList[T] =
      if (remaining.isEmpty) accumulator
      else fromTailRec(remaining.tail, remaining.head :: accumulator)

    fromTailRec(iterable, RNil).reverse
  }

}
