object recfun {
    
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
                                                  //> pascal: (c: Int, r: Int)Int

  pascal(2,5)                                     //> res0: Int = 10
  pascal(2,7)                                     //> res1: Int = 21



  def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars: List[Char], x: Int): Boolean = {
      if (x < 0) false
      else if (chars.isEmpty) if (0 == x) true else false
      else if (chars.head == '(') innerBalance(chars.tail, x+1)
      else if (chars.head == ')') innerBalance(chars.tail, x-1)
      else innerBalance(chars.tail, x)
     }
     
     innerBalance(chars, 0)
  }                                               //> balance: (chars: List[Char])Boolean
  balance("he)((l())( (())ll)llo".toList)         //> res2: Boolean = false
  
  
  
  
  def countChange(money: Int, coins: List[Int]): Int = {
    def countRemainingChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countRemainingChange(money-coins.head, coins) + countRemainingChange(money, coins.tail)
    }
    
    if (money == 0) 0
    else countRemainingChange(money, coins)
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  
  countChange(6, List(1,2))                       //> res3: Int = 4
  
}