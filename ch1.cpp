#include <cstdlib>
#include <iostream>
#include <cmath>
#include <exception>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

using namespace std;

namespace SICP
{

// 1.1.1 The Elements of Programming - Expressions
void section_1_1_1()
{
   cout << 486 << endl;
   cout << 137 + 349 << endl;
   cout << 1000 - 334 << endl;
   cout << 5 * 99 << endl;
   cout << 10 / 5 << endl;
   cout << 2.7 + 10.0 << endl;
   cout << 21 + 35 + 12 + 7 << endl;
   cout << 25 * 4 * 12 << endl;
   cout << 3*5 + 10 - 6 << endl;
   cout << 3*(2*4 + 3 + 5) + 10 - 7 + 6 << endl;
}

// 1.1.2 The Elements of Programming - Naming and the Environment
void section_1_1_2()
{
   int size = 2;
   cout << size << endl;
   cout << 5 * size << endl;
   double pi = 3.14159;
   double radius = 10.0;
   cout << pi * radius * radius << endl;
   double circumference = 2.0 * pi * radius;
   cout << circumference << endl;
}

// 1.1.3 The Elements of Programming - Evaluating Combinations
void section_1_1_3()
{
   cout << (2 + 4*6) * (3 + 5 + 7) << endl;
}

// 1.1.4 The Elements of Programming - Compound Procedures
template <class Number> Number square(Number x) { return x * x; }
template <class Number> Number sum_of_squares(Number x, Number y) { return square(x) + square(y); }
template <class Number> Number f(Number a) { return sum_of_squares(a + 1, a * 2); }
void section_1_1_4()
{
   cout << square(21) << endl;
   cout << square(7 + 5) << endl;
   cout << square(square(3)) << endl;
   cout << sum_of_squares(3, 4) << endl;
   cout << f(5) << endl;
}

// 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
void section_1_1_5()
{
   cout << f(5) << endl;
   cout << sum_of_squares(5 + 1, 5 * 2) << endl;
   cout << square(6) + square(10) << endl;
   cout << (6 * 6) + (10 * 10) << endl;
   cout << 36 + 100 << endl;
   cout << f(5) << endl;
   cout << sum_of_squares(5 + 1, 5 * 2) << endl;
   cout << square(5 + 1) + square(5 * 2) << endl;

   cout << ((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2)) << endl;
   cout << (6 * 6) + (10 * 10) << endl;
   cout << 36 + 100 << endl;
   cout << 136 << endl;
}

// 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
template <class Number> Number abs(Number x)
{
   if (x > 0)
       return x;
   else if (x == 0)
       return 0;
   else
       return -x;
}
template <class Number> Number abs_1(Number x)
{
   if (x < 0)
       return -x;
   else
       return x;
}
template <class T> bool ge(T x, T y) { return (x > y) || (x == y); }
template <class T> bool ge_1(T x, T y) { return !(x < y); }

// Exercise 1.4
template <class Number> Number a_plus_abs_b(Number a, Number b)
{
   if (b > 0)
       return a + b;
   else
       return a - b;
}

// Exercise 1.5
template <class T> T p() { return p<T>(); }
template <class T> T test(T x, T y)
{
   if (x == 0)
       return 0;
   else
       return y;
}

void section_1_1_6()
{
   int x = 6;
   cout << ((x > 5) && (x < 10)) << endl;

   // Exercise 1.1
   cout << 10 << endl;
   cout << 5 + 3 + 4 << endl;
   cout << 9 - 1 << endl;
   cout << 6 / 2 << endl;
   cout << 2*4 + (4 - 6) << endl;
   int a = 3;
   int b = a + 1;
   cout << a + b + a*b << endl;
   cout << (a == b) << endl;
   cout << (((b > a) && (b < (a * b))) ? b : a) << endl;
   cout << ((a == 4) ? 6 : ((b == 4) ? (6 + 7 + a) : 25)) << endl;
   cout << 2 + ((b > a) ? b : a) << endl;
   cout << ((a > b) ? a : ((a < b) ? b : -1)) * (a + 1) << endl;

   // Exercise 1.2
   cout << (5.0 + 4.0 + (2.0 - (3.0 - (6.0 + (4.0 / 5.0))))) /
                     (3.0 * (6.0 - 2.0) * (2.0 - 7.0)) << endl;

   // Exercise 1.5
   // commented out as this is in infinite loop
   // test(0, P());
}

// 1.1.7 The Elements of Programming - Example: square Roots by Newton's Method
template <class Number> Number square_(Number x) { return x * x; }
template <class Real> bool good_enough(Real guess, Real x)
{
   return abs(square(guess) - x) < 0.001;
}
template <class Real> Real average(Real x, Real y)
{
   return (x + y) / 2.0;
}
template <class Real> Real improve(Real guess, Real x)
{
   return average(guess, x / guess);
}
template <class Real> Real sqrt_iter(Real guess, Real x)
{
   if (good_enough(guess, x))
       return guess;
   else
       return sqrt_iter(improve(guess, x), x);
}
template <class Real> Real sqrt(Real x)
{
   return sqrt_iter(1.0, x);
}

/* Exercise 1.6 */
template <class T> T new_if(bool predicate, T then_clause, T else_clause)
{
   if (predicate)
       return then_clause;
   else
       return else_clause;
}
template <class Real> Real sqrt_iter_0(Real guess, Real x)
{
   return
       new_if(
          good_enough(guess, x),
          guess,
          sqrt_iter(improve(guess, x), x));
}

void section_1_1_7()
{
   cout << sqrt(9.0) << endl;
   cout << sqrt(100.0 + 37.0) << endl;
   cout << sqrt(sqrt(2.0) + sqrt(3.0)) << endl;
   cout << square(sqrt(1000.0)) << endl;

   /* Exercise 1.6 */
   cout << new_if((2 == 3), 0, 5) << endl;
   cout << new_if((1 == 1), 0, 5) << endl;
}

// 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
template <class Number> Number square_1(Number x) { return x * x; }
template <class Number> Number doublex(Number x) { return x + x; }
template <class Real> Real square_2(Real x) { return exp(doublex(log(x))); }
template <class Real> bool good_enough_1(Real guess, Real x)
{
   return abs(square_2(guess) - x) < 0.001;
}
template <class Real> Real improve_1(Real guess, Real x)
{
   return average(guess, x / guess);
}
template <class Real> Real sqrt_iter_1(Real guess, Real x)
{
   if (good_enough_1(guess, x))
       return guess;
   else
      return sqrt_iter_1(improve_1(guess, x), x);
}
template <class Real> Real sqrt_1(Real x)
{
   return sqrt_iter_1(1.0, x);
}

// Block-structured
template <class Real> Real sqrt_2(Real x)
{
   struct
   {
      bool good_enough(Real guess, Real x)
      {
         return abs(square(guess) - x) < 0.001;
      }
      Real improve(Real guess, Real x)
      {
         return average(guess, x / guess);
      }
      Real sqrt_iter(Real guess, Real x)
      {
         if (good_enough(guess, x))
             return guess;
         else
            return sqrt_iter(improve(guess, x), x);
      }
   } local;
   return local.sqrt_iter(1.0, x);
}

template <class Number> Number subtract(Number x, Number y) { return x - y; }
template <class Number> Number divide(Number x, Number y) { return x / y; }

// Taking advantage of lexical scoping
template <class Real> Real sqrt_3(Real x)
{
   // CMR Error: Currently stuck trying to figure out boost libraries

   // bool good_enough(Real guess, Real x) { return abs(square(guess) - x) < 0.001; }
   boost::function<bool (Real guess, Real x)> good_enough =
      bind(subtract<Real>, bind(abs<Real>, bind(square<Real>, _1)), _2) < 0.001;

   // Real improve(Real guess, Real x) { return average(guess, x / guess); }
   boost::function<Real (Real guess, Real x)> improve =
      bind(average<Real>, _1, bind(divide<Real>, _2, _1));

   //Real sqrt_iter(Real guess, Real x) { return (good_enough(guess, x)) ? guess : sqrt_iter(improve(guess, x), x); }
   //boost::function<Real (Real guess, Real x)> sqrt_iter =
   //   bind(good_enough<Real>, _1, _2);

   return improve(3.0, 2.0);
}


void section_1_1_8()
{
   cout << square_1(5.0) << endl;
   cout << sqrt_1(25.0) << endl;
   cout << square_2(5.0) << endl;
   cout << sqrt_2(25.0) << endl;
   cout << sqrt_3(25.0) << endl;
}

// 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration
// Recursive
template <class Integer> Integer factorial(Integer n)
{
   if (n == 1)
       return 1;
   else
       return n * factorial(n - 1);
}

// Iterative
template <class Integer> Integer fact_iter(Integer product, Integer counter, Integer max_count)
{
   if (counter > max_count)
       return product;
   else
       return fact_iter(counter * product, counter + 1, max_count);
}
template <class Integer> Integer factorial_1(Integer n)
{
   return fact_iter(1, 1, n);
}

// Iterative, block-structured (from footnote)
template <class Integer> Integer factorial_2(Integer n)
{
   struct
   {
      Integer iter(Integer product, Integer counter)
      {
         return (counter > 5) ? product : iter(counter * product, counter + 1);
      }
   } local;
   return local.iter(1, 1);
}

/* Exercise 1.9 */
template <class Number> Number inc(Number a) { return a + 1; }
template <class Number> Number dec(Number a) { return a - 1; }
template <class Number> Number plus(Number a, Number b)
{
   if (a == 0)
       return b;
   else
       return inc(dec(a) + b);
}
template <class Number> Number plus_1(Number a, Number b)
{
   if (a == 0)
       return b;
   else
       return dec(a) + inc(b);
}

/* Exercise 1.10 */
template <class Number> Number a(Number x, Number y)
{
   if (y == 0)
       return 0;
   else if (x == 0)
       return 2 * y;
   else if (y == 1)
       return 2;
   else
       return a(x - 1, a(x, y - 1));
}
template <class Number> Number f_1(Number n) { return a(0, n); }
template <class Number> Number g_1(Number n) { return a(1, n); }
template <class Number> Number h_1(Number n) { return a(2, n); }
template <class Number> Number k_1(Number n) { return 5 * n * n; }

void section_1_2_1()
{
   cout << factorial(5) << endl;
   cout << factorial_1(5) << endl;
   cout << factorial_2(5) << endl;
   cout << a(1, 10) << endl;
   cout << a(2, 4) << endl;
   cout << a(3, 3) << endl;
}

// 1.2.2 Procedures and the Processes They Generate - Tree Recursion
// Recursive
template <class Integer> Integer fib(Integer n)
{
   switch (n)
   {
       case 0: return 0;
       case 1: return 1;
       default: return fib(n - 1) + fib(n - 2);
   }
}

// Iterative
template <class Integer> Integer fib_iter(Integer a, Integer b, Integer count)
{
   if (count == 0)
       return b;
   else
       return fib_iter(a + b, a, count - 1);
}
template <class Integer> Integer fib_1(Integer n)
{
   return fib_iter(1, 0, n);
}

class SICPException
{
   char* err;
public:
   SICPException(char* s) { err = s; }
   void print() const { cerr << err << endl; }
};


// Counting change
template <class Integer> Integer first_denomination(Integer x)
{
   switch (x)
   {
       case 1: return 1;
       case 2: return 5;
       case 3: return 10;
       case 4: return 25;
       case 5: return 50;
       default: throw SICPException("Domain");
   }
}

template <class Integer> Integer cc(Integer amount, Integer kinds_of_coins)
{
   if (amount == 0)
       return 1;
   else if (amount < 0)
       return 0;
   else if (kinds_of_coins == 0)
       return 0;
   else
       return cc(amount, kinds_of_coins - 1) +
              cc(amount - first_denomination(kinds_of_coins), kinds_of_coins);
}

template <class Integer> Integer count_change(Integer amount)
{
   return cc(amount, 5);
}

/* Exercise 1.15 */
template <class Number> Number cube(Number x) { return x * x * x; }
template <class Real> Real p(Real x) { return (3.0 * x) - (4.0 * cube(x)); }
template <class Real> Real sine(Real angle)
{
   if (!(abs(angle) > 0.1))
       return angle;
   else
       return p(sine(angle / 3.0));
}

void section_1_2_2()
{
   cout << count_change(100) << endl;
}

// 1.2.4 Procedures and the Processes They Generate - Exponentiation
// Linear recursion
template <class Real> Real expt(Real b, Real n)
{
   if (n == 0)
       return 1;
   else
       return b * expt(b, (n - 1));
}

// Linear iteration
template <class Real> Real expt_iter(Real b, Real counter, Real product)
{
   if (counter == 0)
       return product;
   else
       return expt_iter(b, counter - 1, b * product);
}
template <class Real> Real expt_1(Real b, Real n)
{
   return expt_iter(b, n, 1);
}

// Logarithmic iteration
template <class Integer> bool even(Integer n) { return ((n % 2) == 0); }

template <class Integer> Integer Fast_Expt(Integer b, Integer n)
{
   if (n == 0)
       return 1;
   else
       if (Even(n))
           return square(fast_expt(b, n / 2));
       else
           return b * fast_expt(b, n - 1);
}

/* Exercise 1.17 */
template <class Number> Number multiply(Number a, Number b)
{
   if (b == 0)
       return 0;
   else
       return a + (a * (b - 1));
}

/* Exercise 1.19 */
/* exercise left to reader to solve for p' and q'
   template <class Integer> Integer Fib_Iter_(Integer a, Integer b, Integer p, Integer q, Integer count)
   {
       if (count == 0)
           return b;
       else
           if (even(count))
               return fib_iter_(a, b, p', q', count / 2);
           else
               return fib_iter_((b * q) + (a * q) + (a * p), (b * p) + (a * q), p, q, count - 1);
   }
   template <class Integer> Integer fib_2(Integer n)
   {
       return fib_iter_(1, 0, 0, 1, n);
   }
*/

void section_1_2_4() { }

// 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
template <class Integer> Integer gcd(Integer a, Integer b)
{
   if (b == 0)
       return a;
   else
       return gcd(b, a % b);
}
void section_1_2_5() { }

// 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality
// prime
template <class Integer> bool divides(Integer a, Integer b) { return ((b % a) == 0); }

template <class Integer> Integer find_divisor(Integer n, Integer test_divisor)
{
   if (square(test_divisor) > n)
       return n;
   else if (divides(test_divisor, n))
       return test_divisor;
   else
       return find_divisor(n, test_divisor + 1);
}

template <class Integer> Integer smallest_divisor(Integer n) { return find_divisor(n, 2); }

template <class Integer> bool prime(Integer n) { return (n == smallest_divisor(n)); }

// fast_prime
template <class Integer> Integer expmod(Integer nbase, Integer nexp, Integer m)
{
   if (nexp == 0)
       return 1;
   else
       if (even(nexp))
           return square(expmod(nbase, nexp / 2, m)) % m;
       else
           return (nbase * (expmod(nbase, (nexp - 1), m))) % m;
}

// note: using modulus is not necessarily random - but it will do for now
template <class Integer> Integer random_integer(Integer low, Integer high)
{
   return low + rand() % (high - low);
}

template <class Integer> bool fermat_test(Integer n)
{
   struct
   {
      // Note: need to figure out scoping for n
      bool try_it(Integer a, Integer n)
      {
         return (expmod(a, n, n) == a);
      }
   } local;
   return local.try_it(random_integer(0, n), n);
}

template <class Integer> bool fast_prime(Integer n, Integer ntimes)
{
   if (ntimes == 0)
       return true;
   else
       if (fermat_test(n))
           return fast_prime(n, ntimes - 1);
       else
           return false;
}

/* Exercise 1.22 */
boost::posix_time::ptime now()
{
   return boost::posix_time::microsec_clock::local_time();
}
void report_prime(boost::posix_time::time_duration elapsed_time)
{
   cout << " *** " + boost::posix_time::to_simple_string(elapsed_time) << endl;
}
template <class Integer> void start_prime_test(Integer n, boost::posix_time::ptime start_time)
{
   if (prime(n))
       report_prime(now() - start_time);
}
template <class Integer> void timed_prime_test(Integer n)
{
   cout << n << endl;
   start_prime_test(n, now());
}

/* Exercise 1.25 */
template <class Integer> Integer expmod_(Integer nbase, Integer nexp, Integer m)
{
   return fast_expt(nbase, nexp) % m;
}

/* Exercise 1.26 */
template <class Integer> Integer expmod_2(Integer nbase, Integer nexp, Integer m)
{
   if (nexp == 0)
       return 1;
   else
       if (even(nexp))
           return (expmod_2(nbase, (nexp / 2), m) * expmod_2(nbase, (nexp / 2), m)) % m;
       else
           return (nbase * expmod_2(nbase, nexp - 1, m)) % m;
}

void section_1_2_6() { }

// 1.3 Formulating Abstractions with Higher-Order Procedures
template <class Number> Number cube_(Number x) { return x * x * x; }

void section_1_3() { }

// 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
template <class Integer> Integer sum_integers(Integer a, Integer b)
{
   if (a > b)
       return 0;
   else
       return a + sum_integers(a + 1, b);
}

template <class Integer> Integer sum_cubes(Integer a, Integer b)
{
   if (a > b)
       return 0;
   else
       return cube(a) + sum_cubes<Integer>(a + 1, b);
}

template <class Real> Real pi_sum(Real a, Real b)
{
   if (a > b)
       return 0.0;
   else
       return (1.0 / (a * (a + 2.0))) + pi_sum(a + 4.0, b);
}

template <class Integer, class Func> Integer sum(Func term, Integer a, Func next, Integer b)
{
   if (a > b)
       return 0;
   else
       return term(a) + sum<Integer, Func>(term, next(a), next, b);
}

// boost::function<Integer (Integer, Integer)>

// Using sum
template <class Number> Number inc_(Number n) { return n + 1; }

template <class Integer> Integer sum_cubes_(Integer a, Integer b)
{
   return sum(cube, a, inc_, b);
}

int main()
{
   srand(time(0));

   SICP::section_1_1_1();
   SICP::section_1_1_2();
   SICP::section_1_1_3();
   SICP::section_1_1_4();
   SICP::section_1_1_5();
   SICP::section_1_1_6();
   SICP::section_1_1_7();
   SICP::section_1_1_8();
   SICP::section_1_2_1();
   SICP::section_1_2_2();
   SICP::section_1_2_4();
   SICP::section_1_2_5();
   SICP::section_1_2_6();
   SICP::section_1_3();
   SICP::section_1_3_1();
   //SICP::section_1_3_2();
   //SICP::section_1_3_3();
   //SICP::section_1_3_4();
}
