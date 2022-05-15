//// A module containing several different kinds of mathematical functions and constants.
////
//// ---
////
//// * **Standard Mathematical functions**
////   * [`tanh`](#tanh)
////   * [`sin`](#sin)
////   * [`acos`](#acos)
////   * [`acosh`](#acosh)
////   * [`asin`](#asin)
////   * [`atan`](#atan)
////   * [`atan2`](#atan2)
////   * [`atanh`](#atanh)
////   * [`cos`](#cos)
////   * [`cosh`](#cosh)
////   * [`exp`](#exp)
////   * [`ceil`](#ceil)
////   * [`floor`](#floor)
////   * [`pow`](#pow)
////   * [`log`](#log)
////   * [`log10`](#log10)
////   * [`log2`](#log2)
////   * [`sinh`](#sinh)
////   * [`tanh`](#tanh)
////   * [`sign`](#sign)
//// * **Special Mathematical functions**
////   * [`erf`](#erf)
////   * [`gamma`](#gamma)
////   * [`beta`](#beta)
////   * [`round`](#round)
//// * **Combinatorial functions**
////   * [`factorial`](#factorial)
////   * [`combination`](#combination)
////   * [`permutation`](#permutation)
//// * **Mathematical constants**
////   * [`pi`](#pi)
////   * [`tau`](#tau)

import gleam/list
import gleam/int
import gleam/float
import gleam_stats/stats.{amin}
import gleam/io

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The sine function. Wrapped from the Erlang math library.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn sin(x: Float) -> Float {
  do_sin(x)
}

if erlang {
  external fn do_sin(Float) -> Float =
    "math" "sin"
}

if javascript {
  external fn do_sin(Float) -> Float =
    "../gleam_stats.mjs" "sin"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The mathematical constant pi. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn pi() -> Float {
  do_pi()
}

if erlang {
  external fn do_pi() -> Float =
    "math" "pi"
}

if javascript {
  external fn do_pi() -> Float =
    "../gleam_stats.mjs" "pi"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The mathematical constant tau.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn tau() -> Float {
  2. *. pi()
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse cosine function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn acos(x: Float) -> Float {
  do_acos(x)
}

if erlang {
  external fn do_acos(Float) -> Float =
    "math" "acos"
}

if javascript {
  external fn do_acos(Float) -> Float =
    "../gleam_stats.mjs" "acos"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse hyperbolic cosine function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn acosh(x: Float) -> Float {
  do_acosh(x)
}

if erlang {
  external fn do_acosh(Float) -> Float =
    "math" "acosh"
}

if javascript {
  external fn do_acosh(Float) -> Float =
    "../gleam_stats.mjs" "acosh"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse sine function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn asin(x: Float) -> Float {
  do_asin(x)
}

if erlang {
  external fn do_asin(Float) -> Float =
    "math" "asin"
}

if javascript {
  external fn do_asin(Float) -> Float =
    "../gleam_stats.mjs" "asin"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse tangent function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn atan(x: Float) -> Float {
  do_atan(x)
}

if erlang {
  external fn do_atan(Float) -> Float =
    "math" "atan"
}

if javascript {
  external fn do_atan(Float) -> Float =
    "../gleam_stats.mjs" "atan"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse 2-argument tangent function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
/// 
pub fn atan2(y: Float, x: Float) -> Float {
  do_atan2(y, x)
}

if erlang {
  external fn do_atan2(Float, Float) -> Float =
    "math" "atan2"
}

if javascript {
  external fn do_atan2(Float, Float) -> Float =
    "../gleam_stats.mjs" "atan2"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse hyperbolic tangent function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn atanh(x: Float) -> Float {
  do_atanh(x)
}

if erlang {
  external fn do_atanh(Float) -> Float =
    "math" "atanh"
}

if javascript {
  external fn do_atanh(Float) -> Float =
    "../gleam_stats.mjs" "atanh"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The cosine function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cos(x: Float) -> Float {
  do_cos(x)
}

if erlang {
  external fn do_cos(Float) -> Float =
    "math" "cos"
}

if javascript {
  external fn do_cos(Float) -> Float =
    "../gleam_stats.mjs" "cos"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The hyperbolic cosine function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cosh(x: Float) -> Float {
  do_cosh(x)
}

if erlang {
  external fn do_cosh(Float) -> Float =
    "math" "cosh"
}

if javascript {
  external fn do_cosh(Float) -> Float =
    "../gleam_stats.mjs" "cosh"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The exponential function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exp(x: Float) -> Float {
  do_exp(x)
}

if erlang {
  external fn do_exp(Float) -> Float =
    "math" "exp"
}

if javascript {
  external fn do_exp(Float) -> Float =
    "../gleam_stats.mjs" "exp"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The ceiling function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn ceil(x: Float) -> Float {
  do_ceil(x)
}

if erlang {
  external fn do_ceil(Float) -> Float =
    "math" "ceil"
}

if javascript {
  external fn do_ceil(Float) -> Float =
    "../gleam_stats.mjs" "ceil"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The floor function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn floor(x: Float) -> Float {
  do_floor(x)
}

if erlang {
  external fn do_floor(Float) -> Float =
    "math" "floor"
}

if javascript {
  external fn do_floor(Float) -> Float =
    "../gleam_stats.mjs" "floor"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The exponentiation function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn pow(x: Float, y: Float) -> Float {
  do_pow(x, y)
}

if erlang {
  external fn do_pow(Float, Float) -> Float =
    "math" "pow"
}

if javascript {
  external fn do_exp(Float, Float) -> Float =
    "../gleam_stats.mjs" "pow"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The natural logarithm function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn log(x: Float) -> Float {
  do_log(x)
}

if erlang {
  external fn do_log(Float) -> Float =
    "math" "log"
}

if javascript {
  external fn do_log(Float) -> Float =
    "../gleam_stats.mjs" "log"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The base-10 logarithm function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn log10(x: Float) -> Float {
  do_log10(x)
}

if erlang {
  external fn do_log10(Float) -> Float =
    "math" "log10"
}

if javascript {
  external fn do_log10(Float) -> Float =
    "../gleam_stats.mjs" "log10"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The base-2 logarithm function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn log2(x: Float) -> Float {
  do_log2(x)
}

if erlang {
  external fn do_log2(Float) -> Float =
    "math" "log2"
}

if javascript {
  external fn do_log2(Float) -> Float =
    "../gleam_stats.mjs" "log2"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The hyperbolic sine function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn sinh(x: Float) -> Float {
  do_sinh(x)
}

if erlang {
  external fn do_sinh(Float) -> Float =
    "math" "sinh"
}

if javascript {
  external fn do_sinh(Float) -> Float =
    "../gleam_stats.mjs" "sinh"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The hyperbolic tangent function. Wrapped from the Erlang math library.  
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn tanh(x: Float) -> Float {
  do_tanh(x)
}

if erlang {
  external fn do_tanh(Float) -> Float =
    "math" "tanh"
}

if javascript {
  external fn do_tanh(Float) -> Float =
    "../gleam_stats.mjs" "tanh"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The sign function which returns the sign of the input, indicating 
/// whether it is positive, negative, or zero.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn sign(x: Float) -> Float {
  do_sign(x)
}

if erlang {
  fn do_sign(x: Float) -> Float {
    case x <. 0.0 {
      True -> -1.0
      False ->
        case x == 0.0 {
          True -> 0.0
          False -> 1.0
        }
    }
  }
}

if javascript {
  external fn do_sign(Float) -> Float =
    "../gleam_stats.mjs" "sign"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The error function. 
///
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn erf(x: Float) -> Float {
  let [a1, a2, a3, a4, a5] = [
    0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429,
  ]
  let p = 0.3275911

  let sign = case x <. 0.0 {
    True -> -1.0
    False -> 1.0
  }
  let x = float.absolute_value(x)

  // Formula 7.1.26 given in Abramowitz and Stegun.
  let t = 1.0 /. { 1.0 +. p *. x }
  let y =
    1.0 -. { { { { a5 *. t +. a4 } *. t +. a3 } *. t +. a2 } *. t +. a1 } *. t *. exp(
      -1.0 *. x *. x,
    )
  sign *. y
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The gamma function over the real numbers. The function is essentially equal to the 
/// factorial for any positive integer argument: gamma(n) = (n - 1)!.
///
/// The implemented gamma function is approximated through Lanczos approximation
/// using the same coefficients used by the GNU Scientific Library.
/// 
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn gamma(x: Float) -> Float {
  gamma_lanczos(x)
}

const lanczos_g: Float = 7.0

const lanczos_p: List(Float) = [
  0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
  -176.61502916214059, 12.507343278686905, -0.13857109526572012, 0.0000099843695780195716,
  0.00000015056327351493116,
]

fn gamma_lanczos(x: Float) -> Float {
  case x <. 0.5 {
    True -> pi() /. { sin(pi() *. x) *. gamma_lanczos(1.0 -. x) }
    False -> {
      let z = x -. 1.0
      let x: Float =
        list.index_fold(
          lanczos_p,
          0.0,
          fn(acc: Float, v: Float, index: Int) {
            case index > 0 {
              True -> acc +. v /. { z +. int.to_float(index) }
              False -> v
            }
          },
        )
      let t: Float = z +. lanczos_g +. 0.5
      float.power(2.0 *. pi(), 0.5) *. float.power(t, z +. 0.5) *. exp(
        -1.0 *. t,
      ) *. x
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The lower incomplete gamma function over the real numbers.
///
/// The implemented incomplete gamma function is evaluated through a power series
/// expansion.
/// 
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn gammainc(a: Float, x: Float) -> Result(Float, String) {
  case a >. 0.0 && x >=. 0.0 {
    True ->
      float.power(x, a) *. exp(-1.0 *. x) *. gammainc_sum(
        a,
        x,
        1.0 /. a,
        0.0,
        1.0,
      )
      |> Ok
    False -> {
      io.debug(a)
      io.debug(x)
      "Invlaid input argument: a <= 0 or x < 0. Valid input is a > 0 and x >= 0."
      |> Error
    }
  }
}

fn gammainc_sum(a: Float, x: Float, t: Float, s: Float, n: Float) -> Float {
  case t {
    0.0 -> s
    _ -> {
      let ns: Float = s +. t
      let nt: Float = t *. { x /. { a +. n } }
      gammainc_sum(a, x, nt, ns, n +. 1.0)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The beta function over the real numbers.
///
/// The implemented beta function is evaluated through the use of the gamma function.
/// 
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn beta(x: Float, y: Float) -> Float {
  gamma(x) *. gamma(y) /. gamma(x +. y)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function rounds a floating point number to a specific decimal precision.
/// 
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/math
///
///     pub fn example() {
///       math.round(0.4444, 2) |> should.equal(0.44)
///       math.round(0.4445, 2) |> should.equal(0.44)
///       math.round(0.4455, 2) |> should.equal(0.45)
///       math.round(0.4555, 2) |> should.equal(0.46)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn round(x: Float, precision: Int) -> Float {
  let p: Float = float.power(10.0, int.to_float(precision))
  int.to_float(float.round(x *. p)) /. p
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A combinatorial function for computing the total number of combinations of n elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/math
///
///     pub fn example() {
///       // Invalid input gives an error
///       math.factorial(-1)
///       |> should.be_error()
///     
///       // Valid input returns a result
///       math.factorial(0)
///       |> should.equal(Ok(1))
///       math.factorial(1)
///       |> should.equal(Ok(1))
///       math.factorial(2)
///       |> should.equal(Ok(2))
///       math.factorial(3)
///       |> should.equal(Ok(6))
///       math.factorial(4)
///       |> should.equal(Ok(24))
///     }
/// </details>
/// 
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn factorial(n) -> Result(Int, String) {
  case n < 0 {
    True ->
      "Invalid input argument: n < 0. Valid input is n > 0."
      |> Error
    False ->
      case n {
        0 ->
          1
          |> Ok
        1 ->
          1
          |> Ok
        _ ->
          list.range(1, n + 1)
          |> list.fold(1, fn(acc: Int, x: Int) { acc * x })
          |> Ok
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A combinatorial function for computing the number of a k-combinations of n elements.
/// Also known as "n choose k" or the binomial coefficient.
///
/// The implementation uses the effecient iterative multiplicative formula for the computation.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/math
///
///     pub fn example() {
///       // Invalid input gives an error
///       // Error on: n = -1 < 0 
///       math.combination(-1, 1)
///       |> should.be_error()
///     
///       // Valid input returns a result
///       math.combination(4, 0)
///       |> should.equal(Ok(1))
///     
///       math.combination(4, 4)
///       |> should.equal(Ok(1))
///     
///       math.combination(4, 2)
///       |> should.equal(Ok(6))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn combination(n: Int, k: Int) -> Result(Int, String) {
  case n < 0 {
    True ->
      "Invalid input argument: n < 0. Valid input is n > 0."
      |> Error
    False ->
      case k < 0 || k > n {
        True ->
          0
          |> Ok
        False ->
          case k == 0 || k == n {
            True ->
              1
              |> Ok
            False -> {
              assert Ok(min) = amin([int.to_float(k), int.to_float(n - k)])
              list.range(0, float.round(min))
              |> list.fold(
                1,
                fn(acc: Int, x: Int) { acc * { n - x } / { x + 1 } },
              )
              |> Ok
            }
          }
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A combinatorial function for computing the number of k-permuations (without repetitions)
/// of n elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/math
///
///     pub fn example() {
///       // Invalid input gives an error
///       // Error on: n = -1 < 0 
///       math.permutation(-1, 1)
///       |> should.be_error()
///     
///       // Valid input returns a result
///       math.permutation(4, 0)
///       |> should.equal(Ok(1))
///     
///       math.permutation(4, 4)
///       |> should.equal(Ok(1))
///     
///       math.permutation(4, 2)
///       |> should.equal(Ok(12))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn permutation(n: Int, k: Int) -> Result(Int, String) {
  case n < 0 {
    True ->
      "Invalid input argument: n < 0. Valid input is n > 0."
      |> Error
    False ->
      case k < 0 || k > n {
        True ->
          0
          |> Ok
        False ->
          case k == n {
            True ->
              1
              |> Ok
            False -> {
              assert Ok(v1) = factorial(n)
              assert Ok(v2) = factorial(n - k)
              v1 / v2
              |> Ok
            }
          }
      }
  }
}
