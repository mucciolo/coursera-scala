package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int =
    if (c < 0 || c > r) 0
    else if (r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def parenthesesStack(chars: List[Char], height: Int): Int = {
      if (height < 0 || chars.isEmpty) height
      else {
        val inc =	if (chars.head == '(') 1
        else if (chars.head == ')') -1
        else 0

        parenthesesStack(chars.tail, height + inc)
      }
    }

    parenthesesStack(chars, 0) == 0
  }

  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
