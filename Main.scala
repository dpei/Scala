package recfun


/**
 * Solve problems use tail recursion
 * Author: Dong Pei
 * Date: July 21, 2017
 * 
 */



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
      if (c == 0 || c == r) 1  
      else (pascal(c-1, r-1) + pascal(c, r-1))
  

  /**
   * Exercise 2: helper functions are wrapped into function
   */
    def balance(chars: List[Char]): Boolean = {
        def balanced(chars: List[Char], numOpen: Int): Boolean = {
            if (chars.isEmpty) 
              numOpen == 0
            else if (chars.head == '(') 
              balanced(chars.tail, numOpen+1)
            else if (chars.head == ')') 
              // Below condition must satisfy two conditions: 1. one left 
              // parenthesis is there and the balance() function is correct
              numOpen>0 && balanced(chars.tail, numOpen-1)
            else 
              balanced(chars.tail, numOpen)
        }
        balanced(chars, 0)
      }
    /*
    println("balance function is true? "+balance(List[Char]('a','c'))) 
    println("balance function is false? "+balance(List[Char]('(','(')))
    println("balance function is true? "+balance(List[Char]('(',':',')')))
    println("balance function is false? "+balance(List[Char]('(',':','-')))
    */
  /**
   * Exercise 3: This example shows how to put a value of coinNum into another
   * nested function
   */
    
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0)
        0
      else if (coins.isEmpty)
    	  if (money == 0) 1 else 0
    	else
    		countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
    /*
    println("Change 5 with size 4 coin, 0 solution ? "+
        countChange(5, List[Int](4)))
    println("Change 5 with size 1, 2 and 3 coin, 5 solution ? "+
        countChange(5, List[Int](1,2,3)))
    println("Change 5 with size 1 coin, 1 solution ? "+
        countChange(5, List[Int](1)))
    println("Change 1 with size 1, 2 coin, 1 solution ? "+
        countChange(1, List[Int](1,2)))
    println("Change -1 with size 1, 2 coin, 0 solution ? "+
        countChange(-1, List[Int](1,2)))  
          
    */
    
    
  
    
  }
