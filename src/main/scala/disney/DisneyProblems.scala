package me.brunolemus.interview
package disney

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object DisneyProblems {

  private def isPalindrome(word: String): Boolean = {
    @tailrec
    def isPalindromeRec(startIndex: Int, endIndex: Int): Boolean = {
      if (startIndex == endIndex) true
      else if (startIndex > endIndex) true
      else if (word.charAt(startIndex) == word.charAt(endIndex)) isPalindromeRec(startIndex + 1, endIndex - 1)
      else false
    }

    isPalindromeRec(0, word.length - 1)
  }

  private def top10words(book: String): Unit = book.split(" ").foldLeft(new HashMap[String, Int]()) { case (wordsAndAmount, word) =>
    if (!wordsAndAmount.contains(word)) wordsAndAmount + ((word, 1))
    else {
      val amount = wordsAndAmount(word)
      wordsAndAmount + ((word, amount + 1))
    }
  }.toList.sortBy(_._2).take(10) foreach (x => println(x._1))


  def main(args: Array[String]): Unit = {
    println(isPalindrome("vieja"))
    println(isPalindrome("papap"))
    top10words("I am a really big book with a lot of really big words. I hope you like me and we get along fine.")
  }

}
