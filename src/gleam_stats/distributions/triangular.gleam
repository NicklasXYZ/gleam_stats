////<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.15.6/dist/katex.min.css" integrity="sha384-ZPe7yZ91iWxYumsBEOn7ieg8q/o+qh/hQpSaPow8T6BwALcXSCS6C6fSRPIAnTQs" crossorigin="anonymous">
////<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.6/dist/katex.min.js" integrity="sha384-ljao5I1l+8KYFXG7LNEA7DyaFvuvSCmedUf6Y6JI7LJqiu8q5dEivP2nDdFH31V4" crossorigin="anonymous"></script>
////<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.6/dist/contrib/auto-render.min.js" integrity="sha384-+XBljXPPiv+OzfbB3cVmLHf4hdUFHlWNZN5spNQ7rmHTXpd7WvJum6fIACpNNfIR" crossorigin="anonymous"></script>
////<script>
////    document.addEventListener("DOMContentLoaded", function() {
////        renderMathInElement(document.body, {
////          // customised options
////          // • auto-render specific keys, e.g.:
////          delimiters: [
////              {left: '$$', right: '$$', display: false},
////            //   {left: '$', right: '$', display: false},
////            //   {left: '\\(', right: '\\)', display: false},
////              {left: '\\[', right: '\\]', display: true}
////          ],
////          // • rendering keys, e.g.:
////          throwOnError : false
////        });
////    });
////</script>
////<style>
////    .katex { font-size: 1.1em; }
////</style>
////
//// Functions related to continuous triangular random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`triangular_mean`](#triangular_mean)
////   * [`triangular_variance`](#triangular_variance)
////   * [`triangular_pdf`](#triangular_pdf)
////   * [`triangular_cdf`](#triangular_cdf)
////   * [`triangular_random`](#triangular_random)

import gleam/iterator.{Iterator}
import gleam_stats/math

if erlang {
  import gleam/list
  import gleam/pair
  import gleam_stats/distributions/uniform
}

fn check_triangular_parameters(
  a: Float,
  b: Float,
  c: Float,
) -> Result(Bool, String) {
  case a <=. b {
    False ->
      "Invalid input arugment: a > b. Valid input is a <= b."
      |> Error
    True ->
      case a <=. c {
        False ->
          "Invalid input argument: a > c. Valid input is a <= c <= b."
          |> Error
        True ->
          case c <=. b {
            False ->
              "Invalid input argument: c > b. Valid input is a <= c <= b."
              |> Error
            True ->
              True
              |> Ok
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
/// Analytically compute the mean of a continuous triangular random variable   
/// that takes values in the interval $$\[a, b\]$$ and has mode (a peak at value) 
/// $$c$$, $$a \leq c \leq b$$.
///
/// The mean returned is: $$\frac{a + b + c}{3}$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_mean(a: Float, b: Float, c: Float) -> Result(Float, String) {
  case check_triangular_parameters(a, b, c) {
    Error(string) ->
      string
      |> Error
    _ ->
      { a +. b +. c } /. 3.0
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a continuous triangular random variable   
/// that takes values in the intervalinterval $$\[a, b\]$$ and has mode (a peak at value) 
/// $$c$$, $$a \leq c \leq b$$.
///
/// The variance returned is: 
///
/// \\[
/// \frac{a^{2} + b^{2} + c^{2} - a \cdot b - a\cdot c - b \cdot c}{18}
/// \\]
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_variance(
  a: Float,
  b: Float,
  c: Float,
) -> Result(Float, String) {
  case check_triangular_parameters(a, b, c) {
    Error(string) ->
      string
      |> Error
    _ -> {
      assert Ok(v1) = math.pow(a, 2.0)
      assert Ok(v2) = math.pow(b, 2.0)
      assert Ok(v3) = math.pow(c, 2.0)
      { v1 +. v2 +. v3 -. a *. b -. a *. c -. b *. c } /. 18.
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
/// Evaluate, at a certain point $$x \in \(-\infty, \infty\)$$ the probability density function (pdf)
/// of a continuous triangular random variable that takes values in the interval $$\[a, b\]$$ and has
/// mode (a peak at value) $$c$$, $$a \leq c \leq b$$.
///
/// The pdf is defined as:
///
/// \\[
/// f(x; a, b, c) = 
/// \begin{cases}
///  0 &\text{if } x < 0, \\\\
///  \frac{2 \cdot (x - a)}{(b - a) \cdot (c - a)} &\text{if } a \leq x < c, \\\\
///  \frac{2}{b - a} &\text{if } x = c, \\\\
///  \frac{2 \cdot (b - x)}{(b - a) \cdot (b - c)} &\text{if } c < x \leq b, \\\\
///  0 &\text{if } b < x.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/triangular
///     import gleeunit/should
///
///     pub fn example() {
///       // Min value
///       let a: Float = 0.
///       // Max value
///       let b: Float = 1.
///       // The mode of the distribution
///       let c: Float = 0.5
///       // For illustrational purposes, evaluate the pdf at the 
///       // point -100.0
///       triangular.triangular_pdf(-100.0, a, b ,c) 
///       |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_pdf(
  x: Float,
  a: Float,
  b: Float,
  c: Float,
) -> Result(Float, String) {
  do_triangular_pdf(x, a, b, c)
}

if erlang {
  fn do_triangular_pdf(
    x: Float,
    a: Float,
    b: Float,
    c: Float,
  ) -> Result(Float, String) {
    case check_triangular_parameters(a, b, c) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x <. a {
          True ->
            0.0
            |> Ok
          False ->
            case a <=. x && x <. c {
              True ->
                2.0 *. { x -. a } /. { { b -. a } *. { c -. a } }
                |> Ok
              False ->
                case x == c {
                  True ->
                    2.0 /. { b -. a }
                    |> Ok
                  False ->
                    case c <. x && x <=. b {
                      True ->
                        2.0 *. { b -. x } /. { { b -. a } *. { b -. c } }
                        |> Ok
                      False ->
                        0.0
                        |> Ok
                    }
                }
            }
        }
    }
  }
}

if javascript {
  external fn do_triangular_pdf(
    Float,
    Float,
    Float,
    Float,
  ) -> Result(Float, String) =
    "../../triangular.mjs" "triangular_pdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$\(-\infty, \infty\)$$, the cumulative distribution function (cdf) of a 
/// continuous triangular random variable that takes values in the interval $$\[a, b\]$$ and has
/// mode (a peak at value) $$c$$, $$a \leq c \leq b$$.
///
/// The cdf is defined as:
///
/// \\[
/// F(x; a, b, c) = 
/// \begin{cases}
///  0 &\text{if } x \leq 0, \\\\
///  \frac{(x - a)^{2}}{(b - a) \cdot (c - a)} &\text{if } a < x \leq c, \\\\
///  1 - \frac{(b - x)^{2}}{(b - a) \cdot (b - c)} &\text{if } c < x < b, \\\\
///  1 &\text{if } b \leq x.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/triangular
///     import gleeunit/should
///
///     pub fn example() {
///       // Min value
///       let a: Float = 0.
///       // Max value
///       let b: Float = 1.
///       // The mode of the distribution
///       let c: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       triangular.triangular_cdf(-100.0, a, b ,c) 
///       |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_cdf(
  x: Float,
  a: Float,
  b: Float,
  c: Float,
) -> Result(Float, String) {
  do_triangular_cdf(x, a, b, c)
}

if erlang {
  fn do_triangular_cdf(
    x: Float,
    a: Float,
    b: Float,
    c: Float,
  ) -> Result(Float, String) {
    case check_triangular_parameters(a, b, c) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x <=. a {
          True ->
            0.0
            |> Ok
          False ->
            case a <. x && x <=. c {
              True -> {
                assert Ok(v) = math.pow(x -. a, 2.0)
                v /. { { b -. a } *. { c -. a } }
                |> Ok
              }
              False ->
                case c <. x && x <. b {
                  True -> {
                    assert Ok(v) = math.pow(b -. x, 2.0)
                    1.0 -. v /. { { b -. a } *. { b -. c } }
                    |> Ok
                  }
                  False ->
                    1.0
                    |> Ok
                }
            }
        }
    }
  }
}

if javascript {
  external fn do_triangular_cdf(
    Float,
    Float,
    Float,
    Float,
  ) -> Result(Float, String) =
    "../../triangular.mjs" "triangular_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{Z}\_{>0}$$ random numbers in the interval $$\[a, b\]$$ from a 
/// continuous triangular distribution with mode (a peak at value) 
/// $$c$$, $$a \leq c \leq b$$.
///
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generators
///     import gleam_stats/distributions/triangular
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       // Min value
///       let a: Float = 0.
///       // Max value
///       let b: Float = 1.
///       // The mode of the distribution
///       let c: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> triangular.triangular_random(a, b, c, 5_000)
///       let rands: List(Float) = pair.first(out)
///       let stream: Iterator(Int) = pair.second(out)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_random(
  stream: Iterator(Int),
  a: Float,
  b: Float,
  c: Float,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  do_triangular_random(stream, a, b, c, m)
}

if erlang {
  fn do_triangular_random(
    stream: Iterator(Int),
    a: Float,
    b: Float,
    c: Float,
    m: Int,
  ) -> Result(#(List(Float), Iterator(Int)), String) {
    case check_triangular_parameters(a, b, c) {
      Error(string) -> Error(string)
      _ ->
        case m > 0 {
          False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
          True -> {
            let eval: Float = { c -. a } /. { b -. a }
            // Take out 'm' integers from the stream of pseudo-random numbers and generate 
            // uniform random numbers.
            assert Ok(out) = uniform.uniform_random(stream, 0., 1., m)
            // Transform the 'm' continuous uniform random numbers to triangular distributed
            // random numbers.
            let numbers: List(Float) =
              pair.first(out)
              |> list.map(fn(x: Float) -> Float {
                case x <. eval {
                  True -> {
                    assert Ok(v) = math.pow(x *. { b -. a } *. { b -. c }, 0.5)
                    a +. v
                  }
                  False -> {
                    assert Ok(v) =
                      math.pow({ 1. -. x } *. { b -. a } *. { b -. c }, 0.5)
                    b -. v
                  }
                }
              })
            // Then return a tuple consisting of a list of continuous triangular random numbers
            // and the stream of pseudo-random numbers where the 'm' integers have been dropped
            // from the stream.
            #(numbers, pair.second(out))
            |> Ok
          }
        }
    }
  }
}

if javascript {
  external fn do_triangular_random(
    Iterator(Int),
    Float,
    Float,
    Float,
    Int,
  ) -> Result(#(List(Float), Iterator(Int)), String) =
    "../../triangular.mjs" "triangular_random"
}
