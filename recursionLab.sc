//questions 1, 2, 3, 4, 5, 9, 10

object recursionLab {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  def inc(n: Int) = n + 1                         //> inc: (n: Int)Int
	def dec(n: Int) = n - 1                   //> dec: (n: Int)Int
	def isZero(n: Int) = n == 0               //> isZero: (n: Int)Boolean
  
  //1
  def add(n: Int, m: Int):Int = {
  		if (isZero(m)) n
  		else (add(inc(n), dec(m)))
  }                                               //> add: (n: Int, m: Int)Int
  
  add(2,3)                                        //> res0: Int = 5
  add(10,1)                                       //> res1: Int = 11
  add(3,0)                                        //> res2: Int = 3
  add(5,5)                                        //> res3: Int = 10
  
  
  //2
  def mul(n: Int, m: Int): Int = {
  		if (isZero(m)) m
  		else add(n, mul(n,dec(m)))
  }                                               //> mul: (n: Int, m: Int)Int
  
  mul(3,1)                                        //> res4: Int = 3
  mul(5,2)                                        //> res5: Int = 10
  mul(5,3)                                        //> res6: Int = 15
  mul(4,2)                                        //> res7: Int = 8
  mul(10,4)                                       //> res8: Int = 40
  
  
  
  //3
  def exp2(m: Int): Int = {
  		if (isZero(m)) 1
  		else mul(2, exp2(dec(m)))
  }                                               //> exp2: (m: Int)Int
  
  exp2(2)                                         //> res9: Int = 4
  exp2(4)                                         //> res10: Int = 16
  exp2(7)                                         //> res11: Int = 128
  
  
  
  //4
  def hyperExp(n: Int): Int = {
  		if (isZero(n)) 1
  		else exp2(exp2(dec(n)))
  }                                               //> hyperExp: (n: Int)Int
  
  hyperExp(3)                                     //> res12: Int = 16
  hyperExp(2)                                     //> res13: Int = 4

  
  //5
  
  //add tail recursion
  def addTailRecursive(n: Int, m: Int) = {
  		def helper(count: Int, result: Int): Int =
  			if (isZero(count)) result else helper (inc(count), dec(result))
  		helper(n,m)
  }                                               //> addTailRecursive: (n: Int, m: Int)Int
  
  
  addTailRecursive(1,2)                           //> res14: Int = 3
  addTailRecursive(5,4)                           //> res15: Int = 9
  
  
  
  //mul tail recursion
  def mulTailRecursive(n: Int, m: Int) ={
  		if(isZero(m) || isZero(n)) 0
 		else {
    		def helper(count: Int, result: Int): Int =
      		if (count >= m) result else helper(inc(count), add(result, n))
    		helper(1, n)
 		}
	}                                         //> mulTailRecursive: (n: Int, m: Int)Int
  
  mulTailRecursive(3,1)                           //> res16: Int = 3
  mulTailRecursive(5,2)                           //> res17: Int = 10
  mulTailRecursive(5,3)                           //> res18: Int = 15
  
  
  
  
  //exp2 tail recursion
  def exp2TailRecursive(n: Int) = {
 		def helper(count: Int, result: Int): Int =
      			if (n < count) result else helper(inc(count), 2 * result)
  		helper(1, 1)
	}                                         //> exp2TailRecursive: (n: Int)Int
  
  exp2TailRecursive(2)                            //> res19: Int = 4
  exp2TailRecursive(4)                            //> res20: Int = 16
  exp2TailRecursive(7)                            //> res21: Int = 128
  
  
  
  //hyper tail recursion
  def hyperExpTailRecursive(n: Int) = {
 		def helper(count: Int, result: Int): Int =
      			if (n < count) result else helper(inc(count), exp2(exp2(dec(n))))
  		helper(1, 1)
	}                                         //> hyperExpTailRecursive: (n: Int)Int
  
  hyperExpTailRecursive(3)                        //> res22: Int = 16
  hyperExpTailRecursive(2)                        //> res23: Int = 4
  
  
  //9
  
  //recursive
  def fib(n: Int): Int = {
  		if (n <= 2) 1
  		else fib(dec(n)) + fib(dec(dec(n)))
  }                                               //> fib: (n: Int)Int
  
  fib(9)                                          //> res24: Int = 34
     
     
    //tail recursive
    def fibTailRecursive(n: Int) = {
    		def helper(count: Int, result: Int, help: Int): Int =
    			if (n <= 2 || n < count) help else helper(inc(count) , result, result+help)
    		helper(0, 1, 1)
    }                                             //> fibTailRecursive: (n: Int)Int
                                                  
   fibTailRecursive(9)                            //> res25: Int = 11
   
   
   
   	//10
  	 	def choose(n: Int, m: Int): Int = {
   		if(isZero(m)|| n == m) 1
   		else add(choose(dec(n),dec(m	)),choose(dec(n),m))
   	}                                         //> choose: (n: Int, m: Int)Int
  
 		choose(5,2)                       //> res26: Int = 10
}