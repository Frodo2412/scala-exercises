package me.brunolemus.interview
package list


object ListProblems extends App {

  val list = RList from (1 to 100)

  println(list.length)
  println(list(50))
  println(list.reverse)
  println(list ++ (RList from (1 to 10)))
  println(list -- 2)
  println(list map { i => 2 * i })
  println(list flatMap { i => RList from (1 to i) })
  println(list.filter(_ % 2 == 0))

}
