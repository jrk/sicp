// <html>
// <head><title>SICP Chapter #2</title></head>
// <body>
// <script language='JavaScript' type='text/javascript'>
function print(s) { document.writeln(s + "<br>"); }

function addList(x, xs) {
   var zs = xs.slice(0)
   zs.unshift(x);
   return zs;
}

function cons(x, y) { return [x, y]; }
function car(x) { return x[0]; }
function cdr(x) { return x.slice(1); }
function cadr(x) { return x[1]; }

/* Functions defined in previous chapters */
function gcd(a, b) {
   if (b == 0)
      return a;
   else
      return gcd(b, a % b);
}

function fib(n) {
   if (n == 0)
      return 0;
   else if (n == 1)
      return 1;
   else
      return fib(n - 1) + fib(n - 2);
}

function identity(x) { return x; }

// 2 Building Abstractions with Data
function linear_combination(a, b, x, y) {
   return (a * x) + (b * y);
}

function mul(a, b) { return a * b }
function linear_combination_1(a, b, x, y) {
   return mul(a, x) + mul(b, y)
}

// 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers

/* Literal Translation */
   function make_rat(n, d) { return [n, d]; }
   function numer(x) { return x[0]; }
   function denom(x) { return x[1]; }

   function add_rat(x, y) {
      return make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y));
   }

   function sub_rat(x, y) {
      return make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y));
   }

   function mul_rat(x, y) {
      return make_rat(numer(x) * numer(y), denom(x) * denom(y));
   }

   function div_rat(x, y) {
      return make_rat(numer(x) * denom(y), denom(x) * numer(y));
   }

   function equal_rat(x, y) {
      return ((numer(x) * denom(y)) == (numer(y) * denom(x)));
   }

   function cons(x, y) { return [x, y]; }
   function car(x) { return x[0]; }
   function cdr(x) { return x.slice(1); }
   function cadr(x) { return car(cdr(x)); }
   function cadr(x) { return x[1]; }
   var x = cons(1, 2);

   var x = cons(1, 2);
   var y = cons(3, 4);
   var z = cons(x, y);
   print (car(car(z)));
   print (car(cdr(z)));

   // footnote -- alternative definitions
   var make_rat_ = cons
   var numer_ = car
   function compose(f, g) { return function(x) { return f(g(x)); } }
   var denom_ = compose(car, cdr);

   function print_rat(x) {
      print (numer(x) + "/" + denom(x));
   }

   var one_half = make_rat(1, 2);
   print_rat(one_half);

   var one_third = make_rat(1, 3);
   print_rat(add_rat(one_half, one_third));
   print_rat(mul_rat(one_half, one_third));
   print_rat(add_rat(one_third, one_third));

   // reducing to lowest terms in constructor
   function make_rat_1(n, d) {
      var g = gcd(n, d);
      return [n / g, d / g];
   }

   function add_rat_1(x, y) {
      return make_rat_1((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y));
   }

   var one_third = make_rat_1(1, 3);
   print_rat(add_rat_1(one_third, one_third))
/* end Literal Translation */

/* Object Translation */
   function Rational() {
      this.make_rat = function(n, d) { return [n, d]; }
      this.numer = function(x) { return x[0]; }
      this.denom = function(x) { return x[1]; }
      this.add_rat = function(x, y) {
         return this.make_rat((this.numer(x) * this.denom(y)) + (this.numer(y) * this.denom(x)), this.denom(x) * this.denom(y));
      }
      this.sub_rat = function(x, y) {
         return this.make_rat((this.numer(x) * this.denom(y)) - (this.numer(y) * this.denom(x)), this.denom(x) * this.denom(y));
      }
      this.mul_rat = function(x, y) {
         return this.make_rat(this.numer(x) * this.numer(y), this.denom(x) * this.denom(y));
      }
      this.div_rat = function(x, y) {
         return this.make_rat(this.numer(x) * this.denom(y), this.denom(x) * this.numer(y));
      }
      this.equal_rat = function(x, y) {
         return ((this.numer(x) * this.denom(y)) == (this.numer(y) * this.denom(x)));
      }
      this.print_rat = function(x) {
         print (this.numer(x) + "/" + this.denom(x));
      }
   }
   var rational = new Rational();

   var one_half = rational.make_rat(1, 2);
   rational.print_rat(one_half);

   var one_third = rational.make_rat(1, 3);
   rational.print_rat(rational.add_rat(one_half, one_third));
   rational.print_rat(rational.mul_rat(one_half, one_third));
   rational.print_rat(rational.add_rat(one_third, one_third));

   // reducing to lowest terms in constructor
   function Rational_1() {
      this.make_rat = function(n, d) {
         var g = gcd(n, d);
         return [n / g, d / g];
      }
      this.numer = function(x) { return x[0]; }
      this.denom = function(x) { return x[1]; }
      this.add_rat = function(x, y) {
         return this.make_rat((this.numer(x) * this.denom(y)) + (this.numer(y) * this.denom(x)), this.denom(x) * this.denom(y));
      }
      this.sub_rat = function(x, y) {
         return this.make_rat((this.numer(x) * this.denom(y)) - (this.numer(y) * this.denom(x)), this.denom(x) * this.denom(y));
      }
      this.mul_rat = function(x, y) {
         return this.make_rat(this.numer(x) * this.numer(y), this.denom(x) * this.denom(y));
      }
      this.div_rat = function(x, y) {
         return this.make_rat(this.numer(x) * this.denom(y), this.denom(x) * this.numer(y));
      }
      this.equal_rat = function(x, y) {
         return ((this.numer(x) * this.denom(y)) == (this.numer(y) * this.denom(x)));
      }
      this.print_rat = function(x) {
         print (this.numer(x) + "/" + this.denom(x));
      }
   }
   var rational = new Rational_1()

   rational.print_rat(rational.add_rat(one_third, one_third));
/* end Object Translation */


// 2.1.2 Introduction to Data Abstraction - Abstraction barriers

/* Literal Translation */
   // reducing to lowest terms in selectors
   function make_rat_2(n, d) { return cons(n, d); }

   function numer_2(x) {
      var g = gcd(car(x), cadr(x));
      return car(x) / g;
   }

   function denom_2(x) {
      var g = gcd(car(x), cadr(x));
      return cadr(x) / g;
   }

   function add_rat_2(x, y) {
      return make_rat_2((numer_2(x) * denom_2(y)) + (numer_2(y) * denom_2(x)), denom_2(x) * denom_2(y));
   }

   function print_rat_2(x) {
      print (numer_2(x) + "/" + denom_2(x));
   }

   var one_third = make_rat_2(1, 3);
   print_rat_2(add_rat_2(one_third, one_third));
/* end Literal Translation */

/* Module Translation */
   // reducing to lowest terms in selectors
   function Rational_2() {
      this.make_rat = function(n, d) { return [n, d]; }
      this.numer = function(x) {
         var n = car(x);
         var d = cadr(x);
         return n / gcd(n, d);
      }
      this.denom = function(x) {
         var n = car(x);
         var d = cadr(x);
         return d / gcd(n, d);
      }
      this.add_rat = function(x, y) {
         return this.make_rat((this.numer(x) * this.denom(y)) + (this.numer(y) * this.denom(x)), this.denom(x) * this.denom(y));
      }
      this.sub_rat = function(x, y) {
         return this.make_rat((this.numer(x) * this.denom(y)) - (this.numer(y) * this.denom(x)), this.denom(x) * this.denom(y));
      }
      this.mul_rat = function(x, y) {
         return this.make_rat(this.numer(x) * this.numer(y), this.denom(x) * this.denom(y));
      }
      this.div_rat = function(x, y) {
         return this.make_rat(this.numer(x) * this.denom(y), this.denom(x) * this.numer(y));
      }
      this.equal_rat = function(x, y) {
         return ((this.numer(x) * this.denom(y)) == (this.numer(y) * this.denom(x)));
      }
      this.print_rat = function(x) {
         print (this.numer(x) + "/" + this.denom(x));
      }
   }
   var rational = new Rational_2();
/* end Module Translation */

// Exercise 2.2
// exercise left to reader to define appropriate functions
// function print_point(p) {
//   print ("(" + x_point(p) + "," + y_point(p) + ")");
// }

// 2.1.3 Introduction to Data Abstraction - What is meant by data?

function cons_1(x, y) {
   function dispatch(m) {
      if (m == 0)
         return x;
      else if (m == 1)
         return y;
      else
         throw ("Exception: Argument not 0 or 1 -- CONS " + m);
   }
   return dispatch;
}

function car_1(z) { return z(0); }
function cdr_1(z) { return z(1); }

// Exercise 2.4
function cons_2(x, y) {
   return (function(m) { return m(x, y); });
}
function car_2(z) {
   return z(function(p, q) { return p; });
}

// Exercise 2.6
var zero = function(f) { return function(x) { return x; } };
function add1(n) { return function(f) { return function(x) { return (f, ((n, f), x)); } } }

// 2.1.4 Introduction to Data Abstraction - Extended Exercise: Interval Arithmetic

/* Literal Translation */
   // note, javascript does not support tuples
   function make_interval(a, b) { return [a, b]; }

   function lower_bound(pair) { return car(pair); }
   function upper_bound(pair) { return cadr(pair); }

   function add_interval(x, y) {
      return make_interval(lower_bound(x) + lower_bound(y), upper_bound(x) + upper_bound(y));
   }

   function min(x, y) { return (x < y) ? x : y; }
   function max(x, y) { return (x > y) ? x : y; }

   function mul_interval(x, y) {
      var p1 = lower_bound(x) * lower_bound(y)
      var p2 = lower_bound(x) * upper_bound(y)
      var p3 = upper_bound(x) * lower_bound(y)
      var p4 = upper_bound(x) * upper_bound(y)
      return make_interval(
         min(min(p1, p2), min(p3, p4)),
         max(max(p1, p2), max(p3, p4)))
   }

   function div_interval(x, y) {
      var z = make_interval(1.0 / upper_bound(y), 1.0 / lower_bound(y));
      return mul_interval(x, z);
   }

   function make_center_width(c, w) {
      return make_interval(c-w, c+w);
   }

   function center(i) {
      return (lower_bound(i) + upper_bound(i)) / 2.0;
   }

   function width(i) {
      return (upper_bound(i) - lower_bound(i)) / 2.0;
   }

   // parallel resistors
   function par1(r1, r2) {
      return div_interval(mul_interval(r1, r2), add_interval(r1, r2));
   }

   function par2(r1, r2) {
      var one = make_interval(1.0, 1.0);
      return div_interval(one,
               add_interval(div_interval(one, r1),
                            div_interval(one, r2)));
   }
/* end Literal Translation */

/* Object Translation */
   function Interval() {
      this.make_interval = function(a, b) { return [a, b]; }
      this.lower_bound = function(pair) { return car(pair); }
      this.upper_bound = function(pair) { return cadr(pair); }
      this.add_interval = function(x, y) {
         return self.make_interval(self.lower_bound(x) + self.lower_bound(y), self.upper_bound(x) + self.upper_bound(y));
      }
      this.mul_interval = function(x, y) {
         var p1 = self.lower_bound(x) * self.lower_bound(y)
         var p2 = self.lower_bound(x) * self.upper_bound(y)
         var p3 = self.upper_bound(x) * self.lower_bound(y)
         var p4 = self.upper_bound(x) * self.upper_bound(y)
         return self.make_interval(
            min(min(p1, p2), min(p3, p4)),
            max(max(p1, p2), max(p3, p4)))
      }
      this.div_interval = function(x, y) {
         var z = self.make_interval(1.0 / self.upper_bound(y), 1.0 / self.lower_bound(y));
         return self.mul_interval(x, z);
      }
      this.make_center_width = function(c, w) {
         return self.make_interval(c-w, c+w);
      }
      this.center = function(i) {
         return (self.lower_bound(i) + self.upper_bound(i)) / 2.0;
      }
      this.width = function(i) {
         return (self.upper_bound(i) - self.lower_bound(i)) / 2.0;
      }
   }
   var interval = new Interval()

   // parallel resistors
   function par1_(r1, r2) {
      return interval.div_interval(interval.mul_interval(r1, r2), interval.add_interval(r1, r2));
   }

   function par2_(r1, r2) {
      var one = interval.make_interval(1.0, 1.0);
      return interval.div_interval(one,
               interval.add_interval(interval.div_interval(one, r1),
                                     interval.div_interval(one, r2)));
   }
/* end Object Translation */

// 2.2.1 Hierarchical Data and the Closure Property - Representing Sequences

function car_3(x) { return x[0]; }
function cdr_3(x) { return x.slice(1); }

cons(1, cons(2, cons(3, cons(4, []))))
var one_through_four = [1, 2, 3, 4];

print (one_through_four);
print (car(one_through_four));
print (cdr(one_through_four));
print (car(cdr(one_through_four)));
print (addList(10, one_through_four));
print (one_through_four);

// </script>
// </body>
// </html>
