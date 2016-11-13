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
      if((c == 0) ||(c == r)) 1
      else pascal(c-1,r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def isBalanced(chars: List[Char], stack: String) : Boolean= {
        if(chars.isEmpty) stack.isEmpty()
        else if(chars.head == '(') isBalanced(chars.tail, '(' + stack)
        else if(chars.head == ')')
          !stack.isEmpty() && (stack.charAt(0) == '(') && isBalanced(chars.tail, stack.substring(1))
        else isBalanced(chars.tail, stack)
      }
      isBalanced(chars, "")
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(capacity: Int, changes: List[Int]): Int = {
        if(capacity == 0) 1
        else if(capacity < 0) 0
        else if(changes.isEmpty && capacity >= 1 ) 0
        else count(capacity, changes.tail) + count(capacity - changes.head, changes)
      }
      count(money, coins.sortWith(_.compareTo(_) < 0))
    }
  }
