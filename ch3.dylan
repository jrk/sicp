/* Functions defined in previous chapters */
define function gcd_(a :: <integer>, b :: <integer>) => result  :: <integer>;
  if (b == 0)
    a
  else
    gcd_(b, modulo(a, b))
  end if;
end function gcd_;

define function square(x :: <integer>) => result :: <integer>;
  x * x;
end function square;

define function square_double(x :: <double-float>) => result :: <double-float>;
  x * x;
end function square_double;

define function average(x :: <double-float>, y :: <double-float>) => result :: <double-float>;
  (x + y) / 2.0d0;
end function average;

define function abs_double(x :: <double-float>) => result :: <double-float>;
  ((x < 0.0d0) & -x) | x;
end function abs_double;

define function divides(a :: <integer>, b :: <integer>) => result :: <boolean>;
  (modulo(b, a) == 0);
end function divides;

define function find_divisor(n :: <integer>, test_divisor :: <integer>) => result :: <integer>;
  if (square(test_divisor) > n)
    n
  elseif (divides(test_divisor, n))
    test_divisor
  else
    find_divisor(n, test_divisor + 1)
  end if;
end function find_divisor;

define function smallest_divisor(n :: <integer>) => result :: <integer>;
  find_divisor(n, 2);
end function smallest_divisor;

define function prime(n :: <integer>) => result :: <boolean>;
  (n == smallest_divisor(n));
end function prime;

[edit]
// 3.1.1 - Assignment and Local State - Local State Variables
define variable *balance* :: <integer> = 100;

define function withdraw(amount :: <integer>) => result :: type-union(<integer>, <string>);
  if (*balance* >= amount)
    *balance* := *balance* - amount;
    *balance*
  else
    "insufficient funds"
  end if;
end function withdraw;

withdraw(25);
withdraw(25);
withdraw(60);
withdraw(15);

define function new-withdraw();
  let balance :: <integer> = 100;
  method(amount :: <integer>) => result :: type-union(<integer>, <string>);
    if (balance >= amount)
      balance := balance - amount;
    else
      "insufficient funds"
    end if;
  end method;
end function new-withdraw;

define function make-withdraw(balance :: <integer>) => result :: <function>;
  method(amount :: <integer>) => result :: type-union(<integer>, <string>);
    if (balance >= amount)
      balance := balance - amount;
    else
      "insufficient funds"
    end if;
  end method;
end function make-withdraw;

let W1 = make-withdraw(100);
let W2 = make-withdraw(100);

W1(50);
W2(70);
W2(40);
W1(40);

/* Literal Translation */
define function make-account(balance :: <integer>) => result :: <function>;
  local
    method withdraw(amount :: <integer>) => result :: type-union(<integer>, <string>);
      if (balance >= amount)
        balance := balance - amount;
      else
        "insufficient funds"
      end if;
    end,

    method deposit(amount :: <integer>) => result :: type-union(<integer>, <string>);
      balance := balance + amount;
    end,

    method dispatch(m :: <symbol>) => result :: <function>;
      case
        (m = #"withdraw") => withdraw;
        (m = #"deposit")  => deposit;
        otherwise =>
          method(x) concatenate("unknown request: '", as(<string>, m), "'"); end method;
      end case;
    end;

  dispatch;
end function make-account;

let acc = make-account(100);

acc(#"withdraw")(50);
acc(#"withdraw")(60);
acc(#"deposit")(40);
acc(#"withdraw")(60);

let acc2 = make-account(100);

/* Object Translation */
define class <account> (<object>)
  slot balance :: <integer>,
    required-init-keyword: balance:;
end class <account>;

define method withdraw(account :: <account>, amount :: <integer>) => result :: type-union(<integer>, <string>);
  if (balance(account) >= amount)
    balance-setter(balance(account) - amount, account);
    balance(account)
  else
    "insufficient funds"
  end if;
end method withdraw;

define method deposit(account :: <account>, amount :: <integer>) => result :: type-union(<integer>, <string>);
  balance-setter(balance(account) + amount, account);
  balance(account);
end method deposit;

let acc = make(<account>, balance: 100);

withdraw(acc, 50);
withdraw(acc, 60);
deposit(acc, 40);
withdraw(acc, 60);

let acc2 = make(<account>, balance: 100);

/* Exercise 3.1 */
define function make-accumulator(start :: <integer>) => result :: <function>;
  method(x) start := start + x end;
end function make-accumulator;

let A = make-accumulator(5);
format-out("%=\n", A(10));    // 15
format-out("%=\n", A(10));    // 25

/* Exercise 3.2 */
define function make-monitored(f :: <function>) => result :: <function>;
  let counter = 0;
  local
    method dispatch(m :: type-union(<symbol>, <object>)) => result :: <object>;
      case
        (m = #"how-many-calls?") => counter;
        (m = #"reset-counter")   => counter := 0;
        otherwise                => counter := counter + 1; f(m);
      end case;
    end;
  dispatch;
end function make-monitored;

let s = make-monitored(sqrt);

format-out("%=\n", s(100.0d0));
format-out("%=\n", s(100.0d0));
format-out("%=\n", s(100.0d0));
format-out("%=\n", s(#"how-many-calls?"));  // 3
format-out("%=\n", s(#"reset-counter"));
format-out("%=\n", s(100.0d0));         
format-out("%=\n", s(#"how-many-calls?"));  // 1

/* Exercise 3.3 */
define function make-account(balance :: <integer>, account-password :: <symbol>) => result :: <function>;
  local
    method withdraw(password :: <symbol>, amount :: <integer>) => result :: type-union(<integer>, <string>);
      if (password ~= account-password)
        "incorrect password"
      else
        if (balance >= amount)
          balance := balance - amount;
        else
          "insufficient funds"
        end if;
      end if;
    end,

    method deposit(password :: <symbol>, amount :: <integer>) => result :: type-union(<integer>, <string>);
      if (password ~= account-password)
        "incorrect password"
      else
        balance := balance + amount;
      end if;
    end,

    method dispatch(m :: <symbol>) => result :: <function>;
      case
        (m = #"withdraw") => withdraw;
        (m = #"deposit")  => deposit;
        otherwise =>
          method(x) concatenate("unknown request: '", as(<string>, m), "'"); end method;
      end case;
    end;

  dispatch;
end function make-account;

let account = make-account(100, #"abcde");

format-out("%=\n", account(#"deposit")(#"abcde", 50));  // 150
format-out("%=\n", account(#"deposit")(#"abcde", 60));  // 210
format-out("%=\n", account(#"withdraw")(#"abcde", 30)); // 180

format-out("%=\n", account(#"f")(30));                  // unknown request: 'f'
format-out("%=\n", account(#"deposit")(#"XX", 50));     // incorrect password
format-out("%=\n", account(#"withdraw")(#"XY", 30));    // incorrect password

/* Exercise 3.4 */
define constant $max-attempts = 7;

define function call-the-cops() => ();
  0;
end function call-the-cops;

define function make-account(balance :: <integer>, account-password :: <symbol>) => result :: <function>;
  let attempts = 1;

  local
    method withdraw(amount :: <integer>) => result :: type-union(<integer>, <string>);
      if (balance >= amount)
        balance := balance - amount
      else
        "insufficient funds"
      end if;
    end,

    method deposit(amount :: <integer>) => result :: type-union(<integer>, <string>);
      balance := balance + amount;
    end,

    method protected-transaction(password :: <symbol>, transaction :: <function>, amount :: <integer>) => result :: type-union(<integer>, <string>);
      if (attempts > $max-attempts)
        call-the-cops();
        "calling the cops !!!"
      else
        if (password ~= account-password)
          attempts := attempts + 1;
          "incorrect password"
        else
          attempts := 1;
          transaction(amount)
        end if;
      end if;
    end,

    method protected-withdraw(password :: <symbol>, amount :: <integer>) => result :: type-union(<integer>, <string>);
      protected-transaction(password, withdraw, amount);
    end,

    method protected-deposit(password :: <symbol>, amount :: <integer>) => result :: type-union(<integer>, <string>);
      protected-transaction(password, deposit, amount);
    end,

    method dispatch(m :: <symbol>) => result :: <function>;
      case
        (m = #"withdraw") => protected-withdraw;
        (m = #"deposit")  => protected-deposit;
        otherwise =>
          method(x) concatenate("unknown request: '", as(<string>, m), "'"); end method;
      end case;
    end;

  dispatch;
end function make-account;

[edit]
// 3.1.2 - Assignment and Local State - The Benefits of Introducing Assignment
define variable *random-init* :: <integer> = 7;

define function rand-update(x :: <integer>) => result :: <integer>;
  let a = 27;
  let b = 26;
  let m = 127;
  modulo(a * x + b, m);
end function rand-update;

define function rand() => result :: <integer>;
  *random-init* := rand-update(*random-init*);
end function rand;

define function cesaro-test() => result :: <boolean>;
  gcd_(rand(), rand()) == 1;
end function cesaro-test;

define function monte-carlo(trials :: <integer>, experiment :: <function>) => result :: <double-float>;
  local
    method iter(trials-remaining :: <integer>, trials-passed :: <integer>) => result :: <double-float>;
      if (trials-remaining == 0)
        (trials-passed * 1.0d0) / (trials * 1.0d0)
      else
        if (experiment())
          iter(trials-remaining - 1, trials-passed + 1)
        else
          iter(trials-remaining - 1, trials-passed)
        end if;
      end if;
    end;
  iter(trials, 0);
end function monte-carlo;

define function estimate-pi(trials :: <integer>) => result :: <double-float>;
  sqrt(6.0d0 / monte-carlo(trials, cesaro-test));
end function estimate-pi;

format-out("%=\n", estimate-pi(10));

/* second version (accumulator-based, no assignment) */
define function random-gcd-test(trials :: <integer>, initial :: <integer>) => result :: <double-float>;
  local
    method iter(trials-remaining :: <integer>, trials-passed :: <integer>, x :: <integer>) => result :: <double-float>;
      let x1 = rand-update(x);
      let x2 = rand-update(x1);

      if (trials-remaining == 0)
        (trials-passed * 1.0d0) / (trials * 1.0d0)
      else
        if (gcd_(x1, x2) == 1)
          iter(trials-remaining - 1, trials-passed + 1, x2)
        else
          iter(trials-remaining - 1, trials-passed, x2)
        end if;
      end if;
    end;

  iter(trials, 0, initial);
end function random-gcd-test;

define function estimate-pi-v2(trials :: <integer>) => result :: <double-float>;
  sqrt(6.0d0 / random-gcd-test(trials, 7));
end function estimate-pi-v2;

format-out("%=\n", estimate-pi-v2(10));

/* Exercise 3.5 - unfinished */

/* Exercise 3.6 - unfinished */

[edit]
// 3.1.3 - Assignment and Local State - The Cost of Introducing Assignment
define function make-simplified-withdraw(balance :: <integer>) => result :: <function>;
  method(amount :: <integer>) balance := balance - amount; end
end function make-simplified-withdraw;

let w = make-simplified-withdraw(25);
format-out("%=\n", w(20));
format-out("%=\n", w(10));

define function make-decrementer(balance :: <integer>) => result :: <function>;
  method(amount :: <integer>) balance - amount; end
end function make-decrementer;

let d = make-decrementer(25);
format-out("%=\n", d(20));
format-out("%=\n", d(10));

format-out("%=\n", make-decrementer(25)(20));
format-out("%=\n", (method(amount :: <integer>) 25 - amount end)(20));
format-out("%=\n", 25 - 20);

format-out("%=\n", make-simplified-withdraw(25)(20));

/* Sameness and change */
let d1 = make-decrementer(25);
let d2 = make-decrementer(25)

let w3 = make-simplified-withdraw(25);
let w4 = make-simplified-withdraw(25);

format-out("%=\n", w3(20));
format-out("%=\n", w3(20));
format-out("%=\n", w4(20));

let peter-acc = make-account(100);
let paul-acc = make-account(100);

let peter-acc1 = make-account(100);
let paul-acc1 = peter-acc1;

/* Pitfalls of imperative programming */
define function factorial(n :: <integer>) => result :: <integer>;
  local
    method iter(product :: <integer>, counter :: <integer>) => result :: <integer>;
      if (counter > n)
        product
      else
        iter(counter * product, counter + 1)
      end if;
    end;
  iter(1, 1);
end function factorial;

define function factorial1(n :: <integer>) => result :: <integer>;
  let product = 1;
  let counter = 1;
  
  local
    method iter() => result :: <integer>;
      if (counter > n)
        product
      else
        product := counter * product;
        counter := counter + 1;
        iter()
      end if;
    end;

  iter();
end function factorial1;

format-out("%=\n", factorial(5));
format-out("%=\n", factorial1(5));

/* Exercise 3.7 - unfinished */

/* Exercise 3.8 - unfinished */

[edit]
// 3.2.1 - The Environment Model of Evaluation - The Rules for Evaluation
// As above:
//
// define function square(x :: <integer>) => result :: <integer>;
//   x * x;
// end function square;

define variable *square1* = method(x :: <integer>) x * x end;

[edit]
// 3.2.2 - The Environment Model of Evaluation - Applying Simple Procedures
// As above:
//
// define function square(x :: <integer>) => result :: <integer>;
//   x * x;
// end function square;

define function sum-of-squares(x :: <integer>, y :: <integer>) => result :: <integer>;
  square(x) * square(y);
end function sum-of-squares;

define function f(a :: <integer>) => result :: <integer>;
  sum-of-squares(a + 1, a * 2);
end function f;

/* Exercise 3.9 */
define function factorial2(n :: <integer>) => result :: <integer>;
  if (n == 1)
    1
  else
    n * factorial2(n - 1)
  end if;
end function factorial2;

define function fact-iter(product :: <integer>, counter :: <integer>, max-count :: <integer>) => result :: <integer>;
  if (counter > max-count)
    product
  else
    fact-iter(counter * product, counter + 1, max-count)
  end if;
end function fact-iter;

define function factorial3(n :: <integer>) => result :: <integer>;
  fact-iter(1, 1, n);
end function factorial3;

[edit]
// 3.2.3 - The Environment Model of Evaluation - Frames as Repository of Local State
define function make-withdraw1(balance :: <integer>) => result :: <function>;
  method(amount :: <integer>)
    if (balance >= amount)
      balance := balance - amount;
    else
      error(concatenate("insufficient funds ", integer-to-string(balance)));
    end if;
  end
end function make-withdraw1;

let w5 = make-withdraw1(100);
w5(50);

let w6 = make-withdraw1(100);

/* Exercise 3.10 */
define function make-withdraw2(initial-amount :: <integer>) => result :: <function>;
  let balance = initial-amount;
  method(amount :: <integer>)
    if (balance >= amount)
      balance := balance - amount;
    else
      error(concatenate("insufficient funds ", integer-to-string(balance)));
    end if;
  end
end function make-withdraw2;

let w7 = make-withdraw2(100);
w7(50);

let w8 = make-withdraw2(100);

[edit]
// 3.2.4 - The Environment Model of Evaluation - Internal Definitions
/* Same as 1.1.8 */
define function sqrt_3(x :: <double-float>) => result :: <double-float>;
  local
    method good_enough(guess :: <double-float>) => result :: <boolean>;
      abs_double(square_double(guess) - x) < 0.001d0;
    end,

    method improve(guess :: <double-float>) => result :: <double-float>;
      average(guess, x / guess);
    end,

    method sqrt_iter(guess :: <double-float>) => result :: <double-float>;
      if (good_enough(guess))
        guess
      else
        sqrt_iter(improve(guess))
      end if;
    end;

  sqrt_iter(1.0d0);
end function sqrt_3;

/* Exercise 3.11 (very similar to 3.1.1) */
define function make-account(initial-balance :: <integer>) => result :: <function>;
  let balance = initial-balance;

  local
    method withdraw(amount :: <integer>) => result :: <integer>;
      if (balance >= amount)
        balance := balance - amount;
      else
        error(concatenate("insufficient funds ", integer-to-string(balance)));
      end if;
    end,

    method deposit(amount :: <integer>) => result :: <integer>;
      balance := balance + amount;
    end,

    method dispatch(m :: <symbol>) => result :: <function>;
      case
        (m = #"withdraw") => withdraw;
        (m = #"deposit")  => deposit;
        otherwise =>
          error(concatenate("unknown request: '", as(<string>, m), "'"));
      end case;
    end;

  dispatch;
end function make-account;

let acc3 = make-account(50);

acc(#"withdraw")(50);
acc(#"deposit")(40);
acc(#"withdraw")(60);

let acc4 = make-account(100);
