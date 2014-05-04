package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int): Int = {
      if (n == 0)
        1
      else
        n * factorial(n - 1)
    }
    factorial(r) / (factorial(c) * factorial(r - c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(opening: Int, chars: List[Char]): Boolean = {
      if (opening < 0)
        return false
      if (chars.isEmpty)
        return opening == 0

      if (chars.head.equals('('))
        balanceIter(opening + 1, chars.tail)
      else if (chars.head.equals(')'))
        balanceIter(opening - 1, chars.tail)
      else
        balanceIter(opening, chars.tail)

    }
    balanceIter(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def dda(money: Int, c: List[Int]): Int = {
      if (money == 0)
        1
      else if (money < 0 || c.isEmpty)
        0
      else
        dda(money - c.head, c) + dda(money, c.tail)
    }
    dda(money, coins.sorted.reverse)
  }
}
