using System;
   using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SICP
{
class SICP01
{
  public static void Main(string[] args)
  {
      SICP01 me = new SICP01();
      me.Section_1_1_1();
      me.Section_1_1_2();
      me.Section_1_1_3();
      me.Section_1_1_4();
      me.Section_1_1_5();
      me.Section_1_1_6();
      me.Section_1_1_7();
      me.Section_1_1_8();
      me.Section_1_2_1();
      me.Section_1_2_2();
      me.Section_1_2_3();
      me.Section_1_2_4();
      me.Section_1_2_5();
      me.Section_1_2_6();
      me.Section_1_3();
      me.Section_1_3_1();
      me.Section_1_3_2();
      me.Section_1_3_3();
      me.Section_1_3_4();
  }


  
// 1.1.1 The Elements of Programming - Expressions
  void Section_1_1_1()
  {
      Console.WriteLine(486);
      Console.WriteLine(137 + 349);
      Console.WriteLine(1000 - 334);
      Console.WriteLine(5 * 99);
      Console.WriteLine(10 / 5);
      Console.WriteLine(2.7 + 10.0);
      Console.WriteLine(21 + 35 + 12 + 7);
      Console.WriteLine(25 * 4 * 12);
      Console.WriteLine(3 * 5 + 10 - 6);
      Console.WriteLine(3 * (2 * 4 + 3 + 5) + 10 - 7 + 6);
  }


  
// 1.1.2 The Elements of Programming - Naming and the Environment
  void Section_1_1_2()
  {
      var size = 2;
      Console.WriteLine(size);
      Console.WriteLine(5 * size);
      var pi = 3.14159;
      var radius = 10.0;
      Console.WriteLine(pi * radius * radius);
      var circumference = 2.0 * pi * radius;
      Console.WriteLine(circumference);
  }


  
// 1.1.3 The Elements of Programming - Evaluating Combinations
  void Section_1_1_3()
  {
      Console.WriteLine((2 + 4 * 6) * (3 + 5 + 7));
  }


  
// 1.1.4 The Elements of Programming - Compound Procedures
  long Square(long x) { return x * x; }
  long Sum_Of_Squares(long x, long y) { return Square(x) + Square(y); }
  long F(long a) { return Sum_Of_Squares(a + 1, a * 2); }
  void Section_1_1_4()
  {
      Console.WriteLine(Square(21));
      Console.WriteLine(Square(2 + 5));
      Console.WriteLine(Square(Square(3)));
      Console.WriteLine(Sum_Of_Squares(3, 4));
      Console.WriteLine(F(5));
  }


  
// 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
  void Section_1_1_5()
  {
      Console.WriteLine(F(5));
      Console.WriteLine(Sum_Of_Squares(5 + 1, 5 * 2));
      Console.WriteLine(Square(6) + Square(10));
      Console.WriteLine(6 * 6 + 10 * 10);
      Console.WriteLine(36 + 100);
      Console.WriteLine(F(5));
      Console.WriteLine(Sum_Of_Squares(5 + 1, 5 * 2));
      Console.WriteLine(Square(5 + 1) + Square(5 * 2));

      Console.WriteLine(((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2)));
      Console.WriteLine((6 * 6) + (10 * 10));
      Console.WriteLine(36 + 100);
      Console.WriteLine(136);
  }


  
// 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
  double Abs(long x)
  {
      if (x > 0)
          return x;
      else if (x == 0)
          return 0;
      else
          return -x;
  }
  double Abs_1(long x)
  {
      if (x < 0)
          return -x;
      else
          return x;
  }
  bool GE(long x, long y) { return (x > y) || (x == y); }
  bool GE_1(long x, long y) { return !(x < y); }
  void Section_1_1_6()
  {
      var x = 6;
      Console.WriteLine((x > 5) && (x < 10));

      // Exercise 1.1
      Console.WriteLine(10);
      Console.WriteLine(5 + 3 + 4);
      Console.WriteLine(9 - 1);
      Console.WriteLine(6 / 2);
      Console.WriteLine(2 * 4 + 4 - 6);
      var a = 3;
      var b = a + 1;
      Console.WriteLine(a + b + a * b);
      Console.WriteLine(a == b);
      Console.WriteLine(((b > a) && (b < a * b)) ? b : a);
      Console.WriteLine((a == 4) ? 6 : ((b == 4) ? (6 + 7 + a) : 25));
      Console.WriteLine(2 + ((b > a) ? b : a));
      Console.WriteLine(((a > b) ? a : ((a < b) ? b : -1)) * (a + 1));

      // Exercise 1.2
      Console.WriteLine((5.0 + 4.0 + (2.0 - (3.0 - (6.0 + (4.0 / 5.0))))) /
                        (3.0 * (6.0 - 2.0) * (2.0 - 7.0)));

      // Exercise 1.5
      // commented out as this is in infinite loop
      // Test(0, P());
  }

  // Exercise 1.3
  long Three_N(long n1, long n2, long n3)
  {
      if (n1 > n2)
          if (n1 > n3)
              if (n2 > n3)
                  return n1 * n1 + n2 * n2;
              else
                  return n1 * n1 + n3 * n3;
          else
              return n1 * n1 + n3 * n3;
      else
          if (n2 > n3)
              if (n1 > n3)
                  return n2 * n2 + n1 * n1;
              else
                  return n2 * n2 + n3 * n3;
          else
              return n2 * n2 + n3 * n3;
  }

  // Exercise 1.4
  long A_Plus_Abs_B(long a, long b)
  {
      if (b > 0)
          return a + b;
      else
          return a - b;
  }

  // Exercise 1.5
  long P() { return P(); }
  long Test(long x, long y)
  {
      if (x == 0)
          return 0;
      else
          return y;
  }


  
// 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
  double Square(double x) { return x * x; }
  bool Good_Enough(double guess, double x)
  {
      return Math.Abs(Square(guess) - x) < 0.001;
  }
  double Average(double x, double y)
  {
      return (x + y) / 2.0;
  }
  double Improve(double guess, double x)
  {
      return Average(guess, x / guess);
  }
  double Sqrt_Iter(double guess, double x)
  {
      if (Good_Enough(guess, x))
          return guess;
      else
          return Sqrt_Iter(Improve(guess, x), x);
  }
  double Sqrt(double x)
  {
      return Sqrt_Iter(1.0, x);
  }
  void Section_1_1_7()
  {
      Console.WriteLine(Sqrt(9));
      Console.WriteLine(Sqrt(100 + 37));
      Console.WriteLine(Sqrt(Sqrt(2) + Sqrt(3)));
      Square(Sqrt(1000.0));

      // Exercise 1.6
      Console.WriteLine(New_If((2 == 3), 0, 5));
      Console.WriteLine(New_If((1 == 1), 0, 5));
  }

  // Exercise 1.6
  double New_If(bool predicate, double then_clause, double else_clause)
  {
      if (predicate)
          return then_clause;
      else
          return else_clause;
  }
  double Sqrt_Iter_0(double guess, double x)
  {
      return
          New_If(
             Good_Enough(guess, x),
             guess,
             Sqrt_Iter(Improve(guess, x), x));
  }

  // Exercse 1.7
  bool Good_Enough_GP(double guess, double prev)
  {
      return Math.Abs(guess - prev) / guess < 0.001;
  }
  double Sqrt_Iter_GP(double guess, double prev, double x)
  {
      if (Good_Enough_GP(guess, prev))
          return guess;
      else
          return Sqrt_Iter_GP(Improve(guess, x), guess, x);
  }
  double Sqrt_GP(double x)
  {
      return Sqrt_Iter_GP(4.0, 1.0, x);
  }

  // Exercise 1.8 *)
  double Improve_Cube(double guess, double x)
  {
      return (2.0 * guess + x / (guess * guess)) / 3.0;
  }
  double Cube_Iter(double guess, double prev, double x)
  {
      if (Good_Enough_GP(guess, prev))
          return guess;
      else
          return Cube_Iter(Improve_Cube(guess, x), guess, x);
  }
  double Cube_Root_0(double x)
  {
      return Cube_Iter(27.0, 1.0, x);
  }


  
// 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
  double Square_1(double x) { return x * x; }
  double DoubleX(double x) { return x + x; }
  double Square_2(double x) { return Math.Exp(DoubleX(Math.Log(x))); }
  bool Good_Enough_1(double guess, double x)
  {
      return Math.Abs(Square(guess) - x) < 0.001;
  }
  double Improve_1(double guess, double x)
  {
      return Average(guess, x / guess);
  }
  double Sqrt_Iter_1(double guess, double x)
  {
      if (Good_Enough_1(guess, x))
          return guess;
      else return Sqrt_Iter_1(Improve_1(guess, x), x);
  }
  double Sqrt_1(double x)
  {
      return Sqrt_Iter_1(1.0, x);
  }

  // Block-structured
  // Note: Recursive lambdas require separation of declaration and assignment
  // Note: Lambda parameters can not use names defined in containing scope
  double Sqrt_2(double x)
  {
      Func<double, double, bool> Good_Enough =
          (guess, y) =>
          {
              return Math.Abs(Square(guess) - y) < 0.001;
          };
      Func<double, double, double> Improve =
          (guess, y) =>
          {
              return Average(guess, y / guess);
          };
      Func<double, double, double> Sqrt_Iter = null;
      Sqrt_Iter =
          (guess, y) =>
          {
              if (Good_Enough(guess, y))
                  return guess;
              else
                  return Sqrt_Iter(Improve(guess, y), y);
          };
      return Sqrt_Iter(1.0, x);
  }

  // Taking advantage of lexical scoping
  double Sqrt_3(double x)
  {
      Func<double, bool> Good_Enough =
          (guess) =>
          {
              return Math.Abs(Square(guess) - x) < 0.001;
          };
      Func<double, double> Improve =
          (guess) =>
          {
              return Average(guess, x / guess);
          };
      Func<double, double> Sqrt_Iter = null;
      Sqrt_Iter =
          (guess) =>
          {
              if (Good_Enough(guess))
                  return guess;
              else
                  return Sqrt_Iter(Improve(guess));
          };
      return Sqrt_Iter(1.0);
  }

  void Section_1_1_8()
  {
      Console.WriteLine(Square_1(5.0));
      Console.WriteLine(Sqrt_1(25.0));
      Console.WriteLine(Sqrt_2(25.0));
      Console.WriteLine(Sqrt_3(25.0));
  }


  
// 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration
  // Recursive
  long Factorial(long n)
  {
      if (n == 1)
          return 1;
      else
          return n * Factorial(n - 1);
  }

  // Iterative
  long Fact_Iter(long product, long counter, long max_count)
  {
      if (counter > max_count)
          return product;
      else
          return Fact_Iter(counter * product, counter + 1, max_count);
  }
  long Factorial_1(long n)
  {
      return Fact_Iter(1, 1, n);
  }

  // Iterative, block-structured (from footnote)
  long Factorial_2(long n)
  {
      Func<long, long, long> Iter = null;
      Iter =
          (product, counter) =>
          {
              if (counter > n)
                  return product;
              else
                  return Iter(counter * product, counter + 1);
          };
      return Iter(1, 1);
  }

  void Section_1_2_1()
  {
      Console.WriteLine(Factorial(6));

      // Exercise 1.10
      Console.WriteLine(A(1, 10));
      Console.WriteLine(A(2, 4));
      Console.WriteLine(A(3, 3));
  }

  // Exercise 1.9
  long Inc(long a) { return a + 1; }
  long Dec(long a) { return a - 1; }
  long Plus(long a, long b)
  {
      if (a == 0)
          return b;
      else
          return Inc(Dec(a) + b);
  }
  long Plus_1(long a, long b)
  {
      if (a == 0)
          return b;
      else
          return Plus_1(Dec(a), Inc(b));
  }

  // Exercise 1.10
  long A(long x, long y)
  {
      if (y == 0)
          return 0;
      else if (x == 0)
          return 2 * y;
      else if (y == 1)
          return 2;
      else
          return A(x - 1, A(x, y - 1));
  }
  long Fa(long n) { return A(0, n); }
  long Ga(long n) { return A(1, n); }
  long Ha(long n) { return A(2, n); }
  long Ka(long n) { return 5 * n * n; }


  
// 1.2.2 Procedures and the Processes They Generate - Tree Recursion
  // Recursive
  long Fib(long n)
  {
      switch (n)
      {
          case 0: return 0;
          case 1: return 1;
          default: return Fib(n - 1) + Fib(n - 2);
      }
  }

  // Iterative
  long Fib_Iter(long a, long b, long count)
  {
      if (count == 0)
          return b;
      else
          return Fib_Iter(a + b, a, count - 1);
  }
  long Fib_1(long n)
  {
      return Fib_Iter(1, 0, n);
  }

  // Counting change
  long First_Denomination(long x)
  {
      switch (x)
      {
          case 1: return 1;
          case 2: return 5;
          case 3: return 10;
          case 4: return 25;
          case 5: return 50;
          default: throw new Exception("Domain");
      }
  }
  long Cc(long amount, long kinds_of_coins)
  {
      if (amount == 0)
          return 1;
      else if (amount < 0)
          return 0;
      else if (kinds_of_coins == 0)
          return 0;
      else
          return Cc(amount, kinds_of_coins - 1) +
                 Cc(amount - First_Denomination(kinds_of_coins), kinds_of_coins);
  }
  long Count_Change(long amount)
  {
      return Cc(amount, 5);
  }

  void Section_1_2_2()
  {
      Console.WriteLine(Count_Change(100));
  }

  // Exercise 1.11
  long Fb(long n)
  {
      if (n < 3)
          return n;
      else
          return Fb(n - 1) + 2 * Fb(n - 2) + 3 * Fb(n - 3);
  }
  long F_Iter(long a, long b, long c, long count)
  {
      if (count == 0)
          return c;
      else
          return F_Iter(a + 2 * b + 3 * c, a, b, count - 1);
  }
  long Fc(long n)
  {
      return F_Iter(2, 1, 0, n);
  }

  // Exercise 1.12
  long Pascals_Triangle(long n, long k)
  {
      if (n == 0)
          return 1;
      else if (k == 0)
          return 1;
      else if (n == k)
          return 1;
      else
          return Pascals_Triangle(n - 1, k - 1) + Pascals_Triangle(n - 1, k);
  }


  
// 1.2.3 Procedures and the Processes They Generate - Orders of Growth
  void Section_1_2_3() { }

  // Exercise 1.15
  double Cube(double x) { return x * x * x; }
  double P(double x) { return (3.0 * x) - (4.0 * Cube(x)); }
  double sine(double angle)
  {
      if (!(Math.Abs(angle) > 0.1))
          return angle;
      else
          return P(sine(angle / 3.0));
  }


  
// 1.2.4 Procedures and the Processes They Generate - Exponentiation
  // Linear recursion
  long Expt(long b, long n)
  {
      if (n == 0)
          return 1;
      else
          return b * Expt(b, (n - 1));
  }

  // Linear iteration
  long Expt_Iter(long b, long counter, long product)
  {
      if (counter == 0)
          return product;
      else
          return Expt_Iter(b, counter - 1, b * product);
  }
  long Expt_1(long b, long n)
  {
      return Expt_Iter(b, n, 1);
  }

  // Logarithmic iteration
  bool Even(long n) { return ((n % 2) == 0); }
  long Fast_Expt(long b, long n)
  {
      if (n == 0)
          return 1;
      else
          if (Even(n))
              return Square(Fast_Expt(b, n / 2));
          else
              return b * Fast_Expt(b, n - 1);
  }

  void Section_1_2_4() { }

  // Exercise 1.17
  long Multiply(long a, long b)
  {
      if (b == 0)
          return 0;
      else
          return a + (a * (b - 1));
  }

  // Exercise 1.19
  long Fib_Iter_(long a, long b, long p, long q, long count)
  {
      if (count == 0)
          return b;
      else
          if (Even(count))
              return Fib_Iter_(a, b, p * p + q * q, 2 * p * q + q * q, count / 2);
          else
              return Fib_Iter_(b * q + a * q + a * p, b * p + a * q, p, q, count - 1);
  }
  long fib_2(long n)
  {
      return Fib_Iter_(1, 0, 0, 1, n);
  }


  
// 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
  long Gcd(long a, long b)
  {
      if (b == 0)
          return a;
      else
          return Gcd(b, a % b);
  }
  void Section_1_2_5()
  {
      Console.WriteLine(Gcd(40, 6));

      // Exercise 1.20
      Console.WriteLine(Gcd(206, 40));
  }


  
// 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality
  // prime
  bool Divides(long a, long b) { return ((b % a) == 0); }

  long Find_Divisor(long n, long test_divisor)
  {
      if (Square(test_divisor) > n)
          return n;
      else if (Divides(test_divisor, n))
          return test_divisor;
      else
          return Find_Divisor(n, test_divisor + 1);
  }

  long Smallest_Divisor(long n) { return Find_Divisor(n, 2); }

  bool Prime(long n) { return (n == Smallest_Divisor(n)); }

  // fast_prime
  long ExpMod(long nbase, long nexp, long m)
  {
      if (nexp == 0)
          return 1;
      else
          if (Even(nexp))
              return Square(ExpMod(nbase, nexp / 2, m)) % m;
          else
              return (nbase * (ExpMod(nbase, (nexp - 1), m))) % m;
  }

  Random random = new Random();
  bool Fermat_Test(long n)
  {
      Func<long, bool> Try = (a) => (ExpMod(a, n, n) == a);
      return Try((long)Math.Round((double)random.Next((int)n)));
  }

  bool Fast_Prime(long n, long ntimes)
  {
      if (ntimes == 0)
          return true;
      else
          if (Fermat_Test(n))
              return Fast_Prime(n, ntimes - 1);
          else
              return false;
  }

  void Section_1_2_6()
  {
      // Exercise 1.21
      Console.WriteLine(Smallest_Divisor(199));
      Console.WriteLine(Smallest_Divisor(1999));
      Console.WriteLine(Smallest_Divisor(19999));

      // Exercise 1.27
      Console.WriteLine(Carmichael(561));
      Console.WriteLine(Carmichael(1105));
      Console.WriteLine(Carmichael(1729));
      Console.WriteLine(Carmichael(2465));
      Console.WriteLine(Carmichael(2821));
      Console.WriteLine(Carmichael(6601));
  }

  // Exercise 1.22
  void Report_Prime(long elapsed_time)
  {
      Console.WriteLine(" *** " + elapsed_time);
  }
  void Start_Prime_Test(long n, DateTime start_time)
  {
      if (Prime(n))
          Report_Prime(DateTime.Now.Subtract(start_time).Milliseconds);
  }
  void Timed_Prime_Test(long n)
  {
      Console.WriteLine(n);
      Start_Prime_Test(n, DateTime.Now);
  }

  // Exercise 1.25
  long ExpMod_(long nbase, long nexp, long m)
  {
      return Fast_Expt(nbase, nexp) % m;
  }

  // Exercise 1.26
  long ExpMod_2(long nbase, long nexp, long m)
  {
      if (nexp == 0)
          return 1;
      else
          if (Even(nexp))
              return (ExpMod_2(nbase, (nexp / 2), m) * ExpMod_2(nbase, (nexp / 2), m)) % m;
          else
              return (nbase * ExpMod_2(nbase, nexp - 1, m)) % m;
  }

  // Exercise 1.27
  bool Carmichael(long n)
  {
      return Fast_Prime(n, 100) && !Prime(n);
  }


  
// 1.3 Formulating Abstractions with Higher-Order Procedures
  long Cube(long x) { return x * x * x; }
  void Section_1_3() { }

  
// 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
  long Sum_Integers(long a, long b)
  {
      if (a > b)
          return 0;
      else
          return a + Sum_Integers(a + 1, b);
  }

  long Sum_Cubes(long a, long b)
  {
      if (a > b)
          return 0;
      else
          return Cube(a) + Sum_Cubes(a + 1, b);
  }

  double Pi_Sum(double a, double b)
  {
      if (a > b)
          return 0.0;
      else
          return (1.0 / (a * (a + 2.0))) + Pi_Sum(a + 4.0, b);
  }

  long Sum(Func<long, long> term, long a, Func<long, long> next, long b)
  {
      if (a > b)
          return 0;
      else
          return term(a) + Sum(term, next(a), next, b);
  }

  // Using sum
  long Inc_(long n) { return n + 1; }

  long Sum_Cubes_1(long a, long b)
  {
      return Sum(Cube, a, Inc_, b);
  }

  long Identity(long x) { return x; }

  long Sum_Integers_(long a, long b)
  {
      return Sum(Identity, a, Inc_, b);
  }

  double Sum(Func<double, double> term, double a, Func<double, double> next, double b)
  {
      if (a > b)
          return 0.0;
      else
          return term(a) + Sum(term, next(a), next, b);
  }

  double Pi_Sum_(double a, double b)
  {
      Func<double, double> pi_term = (x) => 1.0 / (x * (x + 2.0));
      Func<double, double> pi_next = (x) => x + 4.0;
      return Sum(pi_term, a, pi_next, b);
  }

  double Integral(Func<double, double> f, double a, double b, double dx)
  {
      Func<double, double> add_dx = (x) => x + dx;
      return Sum(f, a + (dx / 2.0), add_dx, b) * dx;
  }

  // same as above
  double cube(double x) { return x * x * x; }

  void Section_1_3_1()
  {
      Console.WriteLine(Sum_Cubes_1(1, 10));
      Console.WriteLine(Sum_Integers_(1, 10));
      Console.WriteLine(8.0 * Pi_Sum_(1.0, 1000.0));
      Console.WriteLine(Integral(Cube, 0.0, 1.0, 0.01));
      Console.WriteLine(Integral(Cube, 0.0, 1.0, 0.001));

      // Exercise 1.29
      Console.WriteLine(Simpson(Cube, 0.0, 1.0, 100));

      // Exercise 1.30
      Console.WriteLine(Sum_Cubes_2(1, 10));

      // Exercise 1.33
      Filtered_Accumulate((x, y) => x + y, 0, Square, 1, Inc, 5, Prime);
  }

  // Exercise 1.29
  double Simpson(Func<double, double> f, double a, double b, long n)
  {
      var h = Math.Abs(b - a) / (double)n;
      Func<long, Func<long, long>, long, double, double> sum_iter = null;
      sum_iter = (start, next, stop, acc) =>
      {
          if (start > stop)
              return acc;
          else
              return sum_iter(next(start), next, stop, acc + f(a + (double)start * h));
      };
      return h * sum_iter(1, Inc, n, 0.0);
  }

  // Exercise 1.30
  long Sum_Iter(Func<long, long> term, long a, Func<long, long> next, long b, long acc)
  {
      if (a > b)
          return acc;
      else
          return Sum_Iter(term, next(a), next, b, acc + term(a));
  }
  long Sum_Cubes_2(long a, long b)
  {
      return Sum_Iter(Cube, a, Inc, b, 0);
  }

  // Exercise 1.31
  long Product(Func<long, long> term, long a, Func<long, long> next, long b)
  {
      if (a > b)
          return 1;
      else
          return term(a) * Product(term, next(a), next, b);
  }
  long Factorial_3(long n)
  {
      return Product(Identity, 1, Inc, n);
  }

  long Product_Iter(Func<long, long> term, long a, Func<long, long> next, long b, long acc)
  {
      if (a > b)
          return acc;
      else
          return Product_Iter(term, next(a), next, b, acc * term(a));
  }

  // Exercise 1.32
  long Accumulate(Func<long, long, long> combiner, long null_value, Func<long, long> term, long a, Func<long, long> next, long b)
  {
      if (a > b)
          return null_value;
      else
          return combiner(term(a), Accumulate(combiner, null_value, term, next(a), next, b));
  }
  long Sum(long a, long b)
  {
      return Accumulate((x, y) => x + y, 0, Identity, a, Inc, b);
  }
  long Product(long a, long b)
  {
      return Accumulate((x, y) => x * y, 1, Identity, a, Inc, b);
  }

  long Accumulate_Iter(Func<long, long, long> combiner, Func<long, long> term, long a, Func<long, long> next, long b, long acc)
  {
      if (a > b)
          return acc;
      else
          return Accumulate_Iter(combiner, term, next(a), next, b, combiner(acc, term(a)));
  }
  long Sum_(long a, long b)
  {
      return Accumulate_Iter((x, y) => x + y, Identity, a, Inc, b, 0);
  }
  long product(long a, long b)
  {
      return Accumulate_Iter((x, y) => x * y, Identity, a, Inc, b, 1);
  }

  // Exercise 1.33
  long Filtered_Accumulate(Func<long, long, long> combiner, long null_value, Func<long, long> term, long a, Func<long, long> next, long b, Func<long, bool> pred)
  {
      if (a > b)
          return null_value;
      else
          if (pred(a))
              return combiner(term(a), Filtered_Accumulate(combiner, null_value, term, next(a), next, b, pred));
          else
              return Filtered_Accumulate(combiner, null_value, term, next(a), next, b, pred);
  }


  
// 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
  double Pi_Sum_2(double a, double b)
  {
      return Sum((x) => 1.0 / (x * (x + 2.0)), a, (x) => x + 4.0, b);
  }

  double Integral_(Func<double, double> f, double a, double b, double dx)
  {
      return Sum(f, a + (dx / 2.0), (x) => x + dx, b) * dx;
  }

  long Plus4(long x) { return x + 4; }

  // Using let
  long Fd(long x, long y)
  {
      Func<long, long, long> f_helper = (a, b) => (x * Square(a)) + (y * b) + (a * b);
      return f_helper(1 + (x * y), 1 - y);
  }

  long Fe(long x, long y)
  {
      Func<long, long, long> fx = (a, b) => (x * Square(a)) + (y * b) + (a * b);
      return fx(1 + (x * y), 1 - y);
  }

  long Ff(long x, long y)
  {
      var a = 1 + (x * y);
      var b = 1 - y;
      return (x * Square(a)) + (y * b) + (a * b);
  }

  long Fg(long x, long y)
  {
      var a = 1 + (x * y);
      var b = 1 - y;
      return (x + Square(a)) + (y * b) + (a * b);
  }

  void Section_1_3_2()
  {
      Func<long, long> plus4 = (x) => x + 4;

      // Note:  Haven't figured a way to get around assigning function to a variable
      // Console.WriteLine(((x, y, z) => x + y + Square(z))(1, 2, 3));
      Func<long, long, long, long> f1 = (x, y, z) => x + y + Square(z);
      Console.WriteLine(f1(1, 2, 3));

      /* C# does not have let binding, so I'm not sure how to best translate these snippets
          var x = 5;
          {
              var x = 3;
              x + (x * 10);
          } + x;

          var x = 2;
          {
              var y = x + 2;
              {
                  var x = 3;
                  x * y;
               };
          };
      */

      Console.WriteLine(Fh(Square));
      Console.WriteLine(Fh((z) => z * (z + 1)));
  }

  // Exercise 1.34
  long Fh(Func<long, long> g) { return g(2); }


  
// 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods
  // Half-interval method
  bool Close_Enough(double x, double y)
  {
      return (Math.Abs(x - y) < 0.001);
  }

  bool Positive(double x) { return (x >= 0.0); }
  bool Negative(double x) { return !Positive(x); }

  double Search(Func<double, double> f, double neg_point, double pos_point)
  {
      var midpoint = Average(neg_point, pos_point);
      if (Close_Enough(neg_point, pos_point))
          return midpoint;
      else
      {
          var test_value = f(midpoint);
          if (Positive(test_value))
              return Search(f, neg_point, midpoint);
          else if (Negative(test_value))
              return Search(f, midpoint, pos_point);
          else
              return midpoint;
      }
  }

  double Half_Interval_Method(Func<double, double> f, double a, double b)
  {
      var a_value = f(a);
      var b_value = f(b);
      if (Negative(a_value) && Positive(b_value))
          return Search(f, a, b);
      else if (Negative(b_value) && Positive(a_value))
          return Search(f, b, a);
      else throw new Exception("Values are not of opposite sign" + a + " " + b);
  }

  // Fixed points
  double tolerance = 0.00001;

  double Fixed_Point(Func<double, double> f, double first_guess)
  {
      Func<double, double, bool> Close_Enough = (v1, v2) => Math.Abs(v1 - v2) < tolerance;
      Func<double, double> Try = null;
      Try =
          (guess) =>
          {
              var next = f(guess);
              if (Close_Enough(guess, next))
                  return next;
              else
                  return Try(next);
          };
      return Try(first_guess);
  }

  // Note: this diverges
  double Sqrt_4(double x)
  {
      return Fixed_Point((y) => x / y, 1.0);
  }

  double Sqrt_5(double x)
  {
      return Fixed_Point((y) => Average(y, x / y), 1.0);
  }

  void Section_1_3_3()
  {
      Console.WriteLine(Half_Interval_Method(Math.Sin, 2.0, 4.0));
      Console.WriteLine(Half_Interval_Method((x) => (x * x * x) - (2.0 * x) - 3.0, 1.0, 2.0));
      Console.WriteLine(Fixed_Point(Math.Cos, 1.0));
      Console.WriteLine(Fixed_Point((y) => Math.Sin(y) + Math.Cos(y), 1.0));
      Console.WriteLine(Sqrt_5(25));

      // Exercise 1.36
      // 35 guesses before convergence
      Console.WriteLine(Fixed_Point((x) => Math.Log(1000.0) / Math.Log(x), 1.5));
      // 11 guesses before convergence
      Console.WriteLine(Fixed_Point(Average_Damp((x) => Math.Log(1000.0) / Math.Log(x)), 1.5));
  }

  // Exercise 1.35
  double goldenRatio()
  {
      return Fixed_Point((x) => 1.0 + 1.0 / x, 1.0);
  }

  // Exercise 1.37
  /* exercise left to reader to define cont_frac
      Console.WriteLine(Cont_Frac((i) => 1.0, (i) => 1.0, k));
  */


  
// 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
  Func<double, double> Average_Damp(Func<double, double> f)
  {
      return (x) => Average(x, f(x));
  }

  double Sqrt_6(double x)
  {
      return Fixed_Point(Average_Damp((y) => x / y), 1.0);
  }

  double Cube_Root(double x)
  {
      return Fixed_Point(Average_Damp((y) => x / Square(y)), 1.0);
  }

  // Newton's method
  double dx = 0.00001;
  Func<double, double> Deriv(Func<double, double> g)
  {
      return (x) => (g(x + dx) - g(x)) / dx;
  }

  double Cube_1(double x) { return x * x * x; }

  Func<double, double> Newton_Transform(Func<double, double> g)
  {
      return (x) => x - (g(x) / (Deriv(g)(x)));
  }

  double Newtons_Method(Func<double, double> g, double guess)
  {
      return Fixed_Point(Newton_Transform(g), guess);
  }

  double Sqrt_7(double x)
  {
      return Newtons_Method((y) => Square(y) - x, 1.0);
  }

  // Fixed point of transformed function
  double Fixed_Point_Of_Transform(Func<double, double> g, Func<Func<double, double>, Func<double, double>> transform, double guess)
  {
      return Fixed_Point(transform(g), guess);
  }

  double Sqrt_8(double x)
  {
      return Fixed_Point_Of_Transform((y) => x / y, Average_Damp, 1.0);
  }

  double Sqrt_9(double x)
  {
      return Fixed_Point_Of_Transform((y) => Square(y) - x, Newton_Transform, 1.0);
  }

  void Section_1_3_4()
  {
      Console.WriteLine((Average_Damp(Square))(10.0));
      Console.WriteLine((Average_Damp(Square))(10.0));
      Console.WriteLine(Deriv(Cube)(5.0));
      Console.WriteLine(Sqrt_6(25));
      Console.WriteLine(Sqrt_7(25));
      Console.WriteLine(Sqrt_8(25));
      Console.WriteLine(Sqrt_9(25));

      // Exercise 1.40
      Console.WriteLine(Newtons_Method(Cubic(5.0, 3.0, 2.5), 1.0));

      // Exercise 1.41
      Console.WriteLine(DoubleY(Inc)(5));
      Console.WriteLine(DoubleY(DoubleY(Inc))(5));
      Console.WriteLine(DoubleY(DoubleY(DoubleY(Inc)))(5));

      // Exercise 1.42
      Console.WriteLine((Compose(Square, Inc))(6));

      // Exercise 1.43
      Console.WriteLine((Repeated(Square, 2))(5));

      // Exercise 1.44
      Console.WriteLine(Fixed_Point(Smooth((x) => Math.Log(1000.0) / Math.Log(x), 0.05), 1.5));

      // Exercise 1.46
      Console.WriteLine(Fixed_Point_(Average_Damp((x) => Math.Log(1000.0) / Math.Log(x)), 1.5));
  }

  // Exercise 1.40
  Func<double, double> Cubic(double a, double b, double c)
  {
      return (x) => Cube(x) + a * x * x + b * x + c;
  }

  // Exercise 1.41
  Func<long, long> DoubleY(Func<long, long> f)
  {
      return (x) => f(f(x));
  }

  // Exercise 1.42
  Func<long, long> Compose(Func<long, long> f, Func<long, long> g)
  {
      return (x) => f(g(x));
  }

  // Exercise 1.43
  Func<long, long> Repeated(Func<long, long> f, long n)
  {
      Func<long, long, long> iterate = null;
      iterate =
          (arg, i) =>
          {
              if (i > n)
                  return arg;
              else
                  return iterate(f(arg), i + 1);
          };
      return (x) => iterate(x, 1);
  }

  // Exercise 1.44
  Func<double, double> Smooth(Func<double, double> f, double dx)
  {
      return (x) => Average(x, (f(x - dx) + f(x) + f(x + dx)) / 3.0);
  }

  // Exercise 1.46
  Func<double, double> Iterative_Improve(Func<double, double, bool> good_enough, Func<double, double> improve)
  {
      //(double * double -> bool) * (double -> double) -> double -> double

      Func<double, double> iterate = null;
      iterate =
          (guess) =>
          {
              var next = improve(guess);
              if (good_enough(guess, next))
                  return next;
              else
                  return iterate(next);
          };
      return (x) => iterate(x);
  }
  double Fixed_Point_(Func<double, double> f, double first_guess)
  {
      var tolerance = 0.00001;
      Func<double, double, bool> good_enough =
          (v1, v2) =>
          {
              return Math.Abs(v1 - v2) < tolerance;
          };
      return (Iterative_Improve(good_enough, f))(first_guess);
  }
eof
   }
}
