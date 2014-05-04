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
    def fafa(r: List[Int]): List[Int] = {
      if (r.isEmpty)
        List()
      else if (r.size == 1)
        1 :: fafa(r.tail)
      else
        r(0) + r(1) :: fafa(r.tail)
    }

    def getRow(r: Int): List[Int] = {
      if (r == 0)
        List(1)
      else {
        1 :: fafa(getRow(r - 1))
      }
    }
    getRow(r)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(opening: Int, chars: List[Char]): Boolean = {
      if (opening < 0)
        false
      else if (chars.isEmpty)
        opening == 0
      else if (chars.head.equals('('))
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
