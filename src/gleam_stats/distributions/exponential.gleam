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
//// Functions related to continuous exponential random variables.
////
//// ---
////
//// * **Available functions**
////   * [`exponential_mean`](#exponential_mean)
////   * [`exponential_variance`](#exponential_variance)
////   * [`exponential_pdf`](#exponential_pdf)
////   * [`exponential_cdf`](#exponential_cdf)
////   * [`exponential_random`](#exponential_random)

import gleam/iterator.{Iterator}
import gleam_stats/math

if erlang {
  import gleam/pair
  import gleam/list
  import gleam_stats/distributions/uniform
}

fn check_exponential_parameters(lambda: Float) {
  case lambda >. 0.0 {
    False ->
      "Invalid input argument: lambda <= 0. Valid input is lambda > 0."
      |> Error
    True ->
      True
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the mean of a continuous exponential random variable   
/// with given rate parameter $$\lambda \in \(0, +\infty)$$.
///
/// The mean returned is: $$\frac{1}{\lambda}$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential_mean(lambda: Float) -> Result(Float, String) {
  case check_exponential_parameters(lambda) {
    Error(string) ->
      string
      |> Error
    _ ->
      1.0 /. lambda
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a continuous exponential random variable   
/// with given rate parameter $$\lambda \in \(0, +\infty)$$.
///
/// The variance returned is: $$\frac{1}{\lambda^{2}}$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential_variance(lambda: Float) -> Result(Float, String) {
  case check_exponential_parameters(lambda) {
    Error(string) ->
      string
      |> Error
    _ -> {
      assert Ok(v) = math.pow(lambda, 2.0)
      1.0 /. v
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
/// Evaluate, at a certain point $$x \in \(-\infty, +\infty\)$$, the probability density function (pdf)
/// of a continuous exponential random variable with given rate parameter $$\lambda \in \(0, +\infty)$$.
///
/// The pdf is defined as:
///
/// \\[
/// f(x; \lambda) = 
/// \begin{cases}
///  \lambda \cdot e^{-\lambda \cdot x} &\text{if } x \geq 0, \\\\
///  0 &\text{if } x < 0.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/exponential
///     import gleeunit/should
///
///     pub fn example() {
///       let lambda: Float = 1.
///       // For illustrational purposes, evaluate the pdf at the 
///       // point -100.0
///       exponential.exponential_pdf(-100.0, lambda) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential_pdf(x: Float, lambda: Float) -> Result(Float, String) {
  do_exponential_pdf(x, lambda)
}

if erlang {
  fn do_exponential_pdf(x: Float, lambda: Float) -> Result(Float, String) {
    case check_exponential_parameters(lambda) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x >=. 0.0 {
          True ->
            lambda *. math.exp(-1.0 *. lambda *. x)
            |> Ok
          False ->
            0.0
            |> Ok
        }
    }
  }
}

if javascript {
  external fn do_exponential_pdf(Float, Float) -> Result(Float, String) =
    "../../exponential.mjs" "exponential_pdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \(-\infty, +\infty\)$$, the cumulative distribution 
/// function (cdf) of a continuous exponential random variable with given rate parameter 
/// $$\lambda \in \(0, +\infty)$$.
///
/// The cdf is defined as:
///
/// \\[
/// F(x; \lambda) = 
/// \begin{cases}
///  1 - e^{-\lambda \cdot x} &\text{if } x \geq 0, \\\\
///  0 &\text{if } x < 0.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/exponential
///     import gleeunit/should
///
///     pub fn example() {
///       let lambda: Float = 1.
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       exponential.exponential_cdf(-100.0, lambda) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential_cdf(x: Float, lambda: Float) -> Result(Float, String) {
  do_exponential_cdf(x, lambda)
}

if erlang {
  fn do_exponential_cdf(x: Float, lambda: Float) -> Result(Float, String) {
    case check_exponential_parameters(lambda) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x >=. 0.0 {
          True ->
            1.0 -. math.exp(-1.0 *. lambda *. x)
            |> Ok
          False ->
            0.0
            |> Ok
        }
    }
  }
}

if javascript {
  external fn do_exponential_cdf(Float, Float) -> Result(Float, String) =
    "../../exponential.mjs" "exponential_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{Z}\_{>0}$$ random numbers from a continuous exponential distribution 
/// with given rate parameter $$\lambda \in \(0, +\infty)$$.
///
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/exponential
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let lambda: Float = 1.
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> exponential.exponential_random(lambda, 5_000)
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
pub fn exponential_random(
  stream: Iterator(Int),
  lambda: Float,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  do_exponential_random(stream, lambda, m)
}

if erlang {
  fn do_exponential_random(
    stream: Iterator(Int),
    lambda: Float,
    m: Int,
  ) -> Result(#(List(Float), Iterator(Int)), String) {
    case check_exponential_parameters(lambda) {
      Error(string) ->
        string
        |> Error
      _ ->
        case m > 0 {
          False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
          True -> {
            // Take out 'm' integers from the stream of pseudo-random numbers and generate 
            // uniform random numbers.
            assert Ok(out) = uniform.uniform_random(stream, 0., 1., m)
            // Transform the 'm' continuous uniform random numbers to exponential distributed
            // random numbers.
            let numbers: List(Float) =
              pair.first(out)
              |> list.map(fn(x: Float) -> Float {
                assert Ok(x1) = math.log(x)
                1. /. { -1. *. lambda } *. x1
              })
            // Then return a tuple consisting of a list of exponential random numbers
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
  external fn do_exponential_random(
    Iterator(Int),
    Float,
    Int,
  ) -> Result(#(List(Float), Iterator(Int)), String) =
    "../../exponential.mjs" "exponential_random"
}
