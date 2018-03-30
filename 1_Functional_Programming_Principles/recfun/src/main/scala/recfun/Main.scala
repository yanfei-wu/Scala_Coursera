package recfun

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
    * Exercise 1: Pascal's triangle
    * The numbers at the edge of the triangle are all 1
    * and each number inside the triangle is the sum of the two numbers above it
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    * verify the balancing of parentheses in a string
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], count: Int): Boolean =
      chars match {
        case Nil => count == 0 // use count to check if all opens are closed in the end
        case '(' :: xs => loop(xs, count + 1)
        case ')' :: xs => count > 0 && loop(xs, count - 1)
        case _ => loop(chars.tail, count)
      }

    loop(chars, 0)
  }

  /**
    * Exercise 3
    * count how many different ways change can be made for an amount with a list of coins
    */
  def countChange(money: Int, coins: List[Int]): Int = coins match {
    case _ if money == 0 => 1
    case h::t if money > 0 => countChange(money - h, h::t) + countChange(money, t)
    case _ => 0
    }
}