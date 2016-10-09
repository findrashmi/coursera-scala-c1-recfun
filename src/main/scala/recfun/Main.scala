package recfun

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
    def pascalIter(acc: Int, col: Int, row: Int): Int = {
      if(row == 0)
        if ((col - row) != 0)
          acc
        else
          acc + 1
      else
        pascalIter(pascalIter(acc,col, row -1), col-1, row-1)
    }

    if((c==0) || (c==r))
      1
    else
      pascalIter(0,c,r)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countParanthesis(acc: Int, chars: List[Char]) : Boolean = {
      if (acc < 0)
        false
      else
        if (chars.size == 0)
          acc == 0
        else
          chars.head match {
            case ')' => countParanthesis(acc - 1, chars.tail)
            case '(' => countParanthesis(acc + 1, chars.tail)
            case _ => countParanthesis(acc, chars.tail)
          }
    }

    countParanthesis(0, chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def makeChange(acc: Int, money: Int, coins: List[Int]): Int = {
      if (coins.size == 0 || money == 0)
        acc
      else
        if (money < 0)
          acc
        else
          if (money - coins.head == 0)
            acc + 1
          else
            makeChange(acc, money - coins.head, coins) + makeChange(acc, money, coins.tail)
    }

    makeChange(0,money, coins.sorted)

  }
}
