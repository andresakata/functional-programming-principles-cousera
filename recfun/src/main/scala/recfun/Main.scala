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
    if (c == 0) 1
    else if (r == 0 && c > 0) 0
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true

    def verifier(acc: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) acc == 0
      else if (acc < 0) false
      else if (chars.head == '(') verifier(acc + 1, chars.tail)
      else if (chars.head == ')') verifier(acc - 1, chars.tail)
      else verifier(acc, chars.tail)
    }

    verifier(0, chars);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
