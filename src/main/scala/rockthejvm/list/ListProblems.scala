package me.brunolemus.interview
package rockthejvm.list

object ListProblems extends App {

  private val unsortedList = 4 :: 6 :: 8 :: 1 :: 3 :: 2 :: 5 :: 7 :: 9 :: 10 :: RNil

  println(unsortedList)
  println(unsortedList.insertSort)
  println(unsortedList.mergeSort)
  println((3 :: RNil).mergeSort)

}