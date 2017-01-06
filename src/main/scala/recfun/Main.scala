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
    def pascal(c: Int, r: Int): Int = 
      if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars: List[Char], x: Int): Boolean = {
      if (x < 0) false
      else if (chars.isEmpty) if (0 == x) true else false
      else if (chars.head == '(') innerBalance(chars.tail, x+1)
      else if (chars.head == ')') innerBalance(chars.tail, x-1)
      else innerBalance(chars.tail, x)
     }
     
     innerBalance(chars, 0)
  }   
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countRemainingChange(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0 || coins.isEmpty) 0
        else countRemainingChange(money-coins.head, coins) + countRemainingChange(money, coins.tail)
      }
    
      if (money == 0) 0
      else countRemainingChange(money, coins)
    } 
  }
