object SICP02 {
   def main(args: Array[String]): unit = {

/* Functions defined in previous chapters */
  def fib(n: long): long =
    n match {
       case 0 => 0;
       case 1 => 1;
       case n => fib(n - 1) + fib(n - 2);
    };
    
  def gcd(a: long, b: long): long =
      b match {
         case 0 => a;
         case b => gcd(b, a % b);
      };
/* 2 Building Abstractions with Data */
  def linear_combination(a: long, b: long, x: long, y: long): long =
    a * x + b * y
    
    
  def mul(x: long,y: long) = (x*y)
  def linear_combination_(a: long, b: long, x: long, y: long): long =
      mul(a,x) + mul(b,y)
/* 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers */
  def make_rat(n: long, d: long) = List(n,d)
  def numer(n: List[long]) = n.head
  def denom(n: List[long]) = n.tail.head
  
  def add_rat(x: List[long], y: List[long]) = 
    make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
      
  def sub_rat(x: List[long], y: List[long]) = 
    make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
    
  def mul_rat(x: List[long], y: List[long]) =
   make_rat(numer(x) * numer(y), denom(x) * denom(y))
    
  def div_rat(x: List[long], y: List[long]) =
   make_rat(numer(x) * denom(y), denom(x) * numer(y))
   
  def equal_rat(x: List[long], y: List[long]) =
   ((numer(x) * denom(y)) == (numer(y) * denom(x)))
  
  def cons[A] = List(_: A, _: A)
  def car[A] = (_: List[A]).head
  def cdr[A] = (_: List[A]).tail
  def cadr[A] = car[A].compose(cdr[A])
  def cadr_[A](x : List[A]) = car(cdr(x))
  
  val x = cons(1,2)
  car(x)
  cdr(x)
  
  val x_ = cons(1, 2)
  val y = cons(3, 4)
  val z = cons(x_, y)
  car(car(z))
  car(cdr(z))
  
  /* footnote -- alternative definitions */
  def make_rat_[A](x: A, y: A) = cons(x,y)
  def numer_[A](x: List[A]) = car(x)
  def denom_[A](x: List[A]) = cdr(x)
  
  def print_rat(x: List[long]) = {
    println()
    print(numer(x))
    print("/")
    print(denom(x))
  }
  val one_half = make_rat(1, 2);
  print_rat(one_half);
  
  val one_third = make_rat(1, 3);
  print_rat(add_rat(one_half, one_third));
  print_rat(mul_rat(one_half, one_third));
  print_rat(add_rat(one_third, one_third));
  
  /* reducing to lowest terms in constructor */
  def make_rat_(n: long, d: long) = {
    val g = gcd(n,d)
    List(n / g, d / g)
  }
  
  def add_rat_(x: List[long], y: List[long]) = 
    make_rat_((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
  
  print_rat(add_rat_(one_third, one_third));
  /* end Literal Translation */
