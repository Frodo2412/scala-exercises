package me.brunolemus.interview
package list


object ListProblems extends App {

  val list = RList from (1 to 100)

  val list2 = 1 :: 2 :: 3 :: 1 :: 3 :: 4 :: 2 :: 2 :: RNil

  println(list.duplicateEach(3))

}
