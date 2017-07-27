import math.abs



object helloworksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val x = 1                                       //> x  : Int = 1
	def increase(i: Int) = i + 1              //> increase: (i: Int)Int
	increase(2)                               //> res0: Int = 3
  
  // function has to be seperated with values
  def product(f: Int=>Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)              //> product: (f: Int => Int)(a: Int, b: Int)Int
  product(x => x*x)(3,4)                          //> res1: Int = 144
  product(x=>x)(3,4)                              //> res2: Int = 12
  
  def fact(n:Int) = product(x => x)(1,n)          //> fact: (n: Int)Int
  fact(5)                                         //> res3: Int = 120
  
  def sum(f: Int=>Int)(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)                  //> sum: (f: Int => Int)(a: Int, b: Int)Int
  sum(x => x)(1,5)                                //> res4: Int = 15
  
  def sumfrom1(n: Int) = sum(x=>x)(1,n)           //> sumfrom1: (n: Int)Int
  sumfrom1(5)                                     //> res5: Int = 15
  
  
  
  
  
  // two function were being used as parameter, two int were being used as parameter
  def mapReduce(f: Int=>Int, combine: (Int, Int) => Int, zero: Int)(a:Int, b:Int): Int =
  	if (a>b) zero
  	else combine(f(a), mapReduce(f, combine, zero)(a+1,b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
  
  def product2(f: Int=>Int)(a: Int, b: Int): Int =
  	mapReduce(f, (x,y)=> x*y, 1)(a,b)         //> product2: (f: Int => Int)(a: Int, b: Int)Int
  product2(x => x*x)(3,4)                         //> res6: Int = 144
  
  
  def sum2(f: Int=>Int)(a: Int, b: Int): Int =
  	mapReduce(f, (x,y)=> x+y, 0)(a,b)         //> sum2: (f: Int => Int)(a: Int, b: Int)Int
  sum2(x=>x)(1,5)                                 //> res7: Int = 15
  
  
  // abstract function when possible
  //
  
  val tolerance = 0.0001                          //> tolerance  : Double = 1.0E-4
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance              //> isCloseEnough: (x: Double, y: Double)Boolean
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
		}
    iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  
  // with this function approaching value 2
  fixedPoint(x=>1+x/2)(1)                         //> res8: Double = 1.999755859375
  def averageDamp(f: Double => Double)(x: Double) = (x+f(x))/2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
  
  
  def sqrt(x: Double) =
  	fixedPoint(averageDamp(y=>x/y))(1)        //> sqrt: (x: Double)Double
  sqrt(2)                                         //> res9: Double = 1.4142135623746899
  
}