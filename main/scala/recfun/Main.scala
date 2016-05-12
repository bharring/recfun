package recfun
import common._

import scala.annotation.tailrec

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
    def factorial(num:Int):Int = {
      if (num <= 1) 1
      else factorial(num - 1) * num
    }
    factorial(r) / (factorial(c) * factorial(r - c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(chars: List[Char], rv:Int):Int = {
      if (rv < 0) rv
      else if (chars.isEmpty) rv
      else if (chars.head == '(') loop(chars.tail, rv + 1)
      else if (chars.head == ')') loop(chars.tail, rv - 1)
      else loop(chars.tail, rv)
    }
    loop(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == coins.head) 1 + countChange(money, coins.tail)
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}