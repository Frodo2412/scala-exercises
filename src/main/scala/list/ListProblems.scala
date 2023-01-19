package me.brunolemus.interview
package list


object ListProblems extends App {

  val list = 1 :: 2 :: 3 :: RNil

  println(list sample 3)
  println(list.sort)

}
