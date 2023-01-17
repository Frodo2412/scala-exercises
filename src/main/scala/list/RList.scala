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
    def removeTailRec(remaining: RList[T], currentIndex: Int, accumulated: RList[T]) : RList[T] = remaining match {
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
